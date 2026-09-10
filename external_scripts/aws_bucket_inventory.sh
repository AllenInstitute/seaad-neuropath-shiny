#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage:
  aws_bucket_inventory.sh --bucket BUCKET [options]

Required:
  -b, --bucket NAME       S3 bucket name (do not include s3://)

Options:
  -o, --output FILE       Output CSV (default: <bucket>_inventory.csv)
  -p, --prefix PREFIX     Only inventory keys under this prefix
      --profile PROFILE   AWS CLI profile to use
      --region REGION     AWS region to use
  -h, --help              Show this help

Examples:
  ./aws_bucket_inventory.sh --bucket my-data-bucket
  ./aws_bucket_inventory.sh -b my-data-bucket -p studies/SEA-AD/ -o sea_ad_inventory.csv
  ./aws_bucket_inventory.sh -b my-data-bucket --profile research --region us-west-2

The CSV contains one row for every inferred folder/prefix and every S3 object.
S3 has no true directories, so folder rows are derived from object keys.
USAGE
}

bucket=""
output=""
prefix=""
profile=""
region=""

while (($#)); do
  case "$1" in
    -b|--bucket)
      [[ $# -ge 2 ]] || { echo "Error: $1 requires a value" >&2; exit 2; }
      bucket=$2
      shift 2
      ;;
    -o|--output)
      [[ $# -ge 2 ]] || { echo "Error: $1 requires a value" >&2; exit 2; }
      output=$2
      shift 2
      ;;
    -p|--prefix)
      [[ $# -ge 2 ]] || { echo "Error: $1 requires a value" >&2; exit 2; }
      prefix=$2
      shift 2
      ;;
    --profile)
      [[ $# -ge 2 ]] || { echo "Error: $1 requires a value" >&2; exit 2; }
      profile=$2
      shift 2
      ;;
    --region)
      [[ $# -ge 2 ]] || { echo "Error: $1 requires a value" >&2; exit 2; }
      region=$2
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Error: unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

[[ -n "$bucket" ]] || { echo "Error: --bucket is required" >&2; usage >&2; exit 2; }
command -v aws >/dev/null 2>&1 || { echo "Error: AWS CLI is not installed" >&2; exit 1; }
command -v python3 >/dev/null 2>&1 || { echo "Error: python3 is not installed" >&2; exit 1; }

if [[ -z "$output" ]]; then
  output="${bucket}_inventory.csv"
fi

python3 - "$bucket" "$output" "$prefix" "$profile" "$region" <<'PY'
import csv
import json
import os
from pathlib import PurePosixPath
import sqlite3
import subprocess
import sys
import tempfile


bucket, output, prefix, profile, region = sys.argv[1:]
output = os.path.abspath(output)
output_dir = os.path.dirname(output)
if output_dir:
    os.makedirs(output_dir, exist_ok=True)

fd, db_path = tempfile.mkstemp(prefix="s3_inventory_", suffix=".sqlite3")
os.close(fd)


def parent_and_name(path, is_folder=False):
    clean = path.rstrip("/") if is_folder else path
    if "/" not in clean:
        return "", clean
    parent, name = clean.rsplit("/", 1)
    return parent + "/", name


def inferred_folders(key):
    """Return every directory prefix implied by an S3 object key."""
    parts = key.split("/")
    folder_count = len(parts) if key.endswith("/") else len(parts) - 1
    current = []
    for part in parts[:folder_count]:
        if not part:
            continue
        current.append(part)
        yield "/".join(current) + "/"


try:
    db = sqlite3.connect(db_path)
    db.execute("PRAGMA journal_mode=WAL")
    db.execute("CREATE TABLE folders (path TEXT PRIMARY KEY)")
    db.execute("""
        CREATE TABLE files (
            path TEXT PRIMARY KEY,
            size_bytes INTEGER,
            last_modified TEXT,
            storage_class TEXT,
            etag TEXT
        )
    """)

    token = None
    page_number = 0
    object_count = 0

    while True:
        command = [
            "aws", "s3api", "list-objects-v2",
            "--bucket", bucket,
            "--output", "json",
            "--no-cli-pager",
            # We paginate explicitly below. Without this option, AWS CLI waits
            # for every API page before emitting JSON, which looks like a hang
            # on large buckets.
            "--no-paginate",
        ]
        if prefix:
            command.extend(["--prefix", prefix])
        if profile:
            command.extend(["--profile", profile])
        if region:
            command.extend(["--region", region])
        if token:
            command.extend(["--continuation-token", token])

        print(
            f"Requesting page {page_number + 1} from s3://{bucket} ...",
            file=sys.stderr,
            flush=True,
        )
        try:
            completed = subprocess.run(
                command,
                check=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
            )
        except subprocess.CalledProcessError as exc:
            message = exc.stderr.strip() or "AWS CLI request failed"
            raise SystemExit(f"Error listing s3://{bucket}: {message}") from exc

        response = json.loads(completed.stdout)
        page_number += 1

        for item in response.get("Contents", []):
            key = item["Key"]
            for folder in inferred_folders(key):
                db.execute("INSERT OR IGNORE INTO folders(path) VALUES (?)", (folder,))

            # Keys ending in '/' are directory-marker objects. Represent them as
            # folders only so they are not duplicated as zero-byte files.
            if not key.endswith("/"):
                db.execute(
                    "INSERT OR REPLACE INTO files VALUES (?, ?, ?, ?, ?)",
                    (
                        key,
                        item.get("Size"),
                        item.get("LastModified", ""),
                        item.get("StorageClass", ""),
                        item.get("ETag", "").strip('"'),
                    ),
                )
                object_count += 1

        db.commit()
        print(
            f"Scanned page {page_number}: {object_count:,} file objects",
            file=sys.stderr,
            flush=True,
        )

        if not response.get("IsTruncated"):
            break
        token = response.get("NextContinuationToken")
        if not token:
            raise SystemExit("Error: AWS returned a truncated response without a continuation token")

    with open(output, "w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow([
            "record_type",
            "bucket",
            "path",
            "parent_prefix",
            "name",
            "suffix",
            "compound_suffix",
            "size_bytes",
            "last_modified",
            "storage_class",
            "etag",
        ])

        folder_count = 0
        for (path,) in db.execute("SELECT path FROM folders ORDER BY path"):
            parent, name = parent_and_name(path, is_folder=True)
            writer.writerow(["folder", bucket, path, parent, name, "", "", "", "", "", ""])
            folder_count += 1

        for path, size, modified, storage, etag in db.execute(
            "SELECT path, size_bytes, last_modified, storage_class, etag "
            "FROM files ORDER BY path"
        ):
            parent, name = parent_and_name(path)
            suffixes = PurePosixPath(name).suffixes
            suffix = suffixes[-1].lower() if suffixes else ""
            compound_suffix = "".join(suffixes).lower()
            writer.writerow([
                "file", bucket, path, parent, name, suffix, compound_suffix,
                size, modified, storage, etag,
            ])

    print(
        f"Wrote {folder_count:,} folders and {object_count:,} files to {output}",
        file=sys.stderr,
    )
finally:
    try:
        db.close()
    except NameError:
        pass
    for candidate in (db_path, db_path + "-wal", db_path + "-shm"):
        try:
            os.remove(candidate)
        except FileNotFoundError:
            pass
PY
