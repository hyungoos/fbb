#!/bin/bash

source ~/venv_fbb/bin/activate

echo "Running add_drop.py..."

>&2 echo "testing stderr"

python /Users/hkang/workspace/fbb/yahoo_api/add_drop.py --add 9107 --drop 9627
python /Users/hkang/workspace/fbb/yahoo_api/add_drop.py --add 9356 --drop 9455

deactivate
