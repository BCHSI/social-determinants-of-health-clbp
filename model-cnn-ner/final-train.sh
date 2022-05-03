TRAIN="data/spacy-level-2/sdoh-cocoa-skippy-no-empty-2022-01-11-fold-0-train.spacy"
DEV="data/spacy-level-2/sdoh-cocoa-skippy-no-empty-2022-01-11-fold-0-test.spacy"
CFG=config-2022-01-27-lr-0.0002.cfg

python -m spacy train $CFG  --code ./functions.py \
        --output "checkpoints-sdoh-2022-01-27-lr-0.0002-final"/ \
        --paths.train $TRAIN \
        --paths.dev $DEV
