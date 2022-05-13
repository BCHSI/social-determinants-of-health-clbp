set -e
set -o xtrace

# INPUTDIR=./sdoh-with-cuis
INPUTDIR=checkpoints-sdoh-2022-01-27-lr-0.0002-final/model-best/
OUTDIR=./packages
mkdir -p $OUTDIR
JSON=sdoh-with-cuis/sdoh_cui/mapping-ontology-to-umls.json

python -m spacy package \
        --force \
        $INPUTDIR  $OUTDIR \
        --code  sdoh-with-cuis/functions.py \
        --name  sdoh_cnn_ner

#        --code  sdoh-with-cuis/functions.py,sdoh-with-cuis/postprocess.py \
        #,sdoh-with-cuis/postprocess.py \
# # sdoh-best/postprocess.py,
# # 
# # postprocess.py
# #spacy package /path/to/model_name /output_dir
# 
pip install packages/en_sdoh_cnn_ner-0.0.0


CUI_MODEL_DIR=packages/sdoh-cnn-with-cuis/

mkdir -p $CUI_MODEL_DIR/sdoh_cui
cp model/mapping-ontology-to-umls.json $CUI_MODEL_DIR/sdoh_cui
# cp $INPUTDIR/sdoh_cui/mapping-ontology-to-umls.json $OUTPUT_DIR/sdoh_cui

python -m spacy assemble \
    config-cui.cfg \
    $CUI_MODEL_DIR \
    --code sdoh-with-cuis/postprocess.py \
    --components.ner.source en_sdoh_cnn_ner 

python -m spacy package \
        --force \
        --code sdoh-with-cuis/postprocess.py \
        --name  sdoh_cnn_ner_cui \
        $CUI_MODEL_DIR \
        packages/
