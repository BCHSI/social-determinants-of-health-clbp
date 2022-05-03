
#ln -s sdoh_bow_cui/lemma_tokenizer.py .

#SOURCE=package/en_sdoh_bow-0.0.2/en_sdoh_bow/en_sdoh_bow-0.0.2/

#cp -r package/en_sdoh_bow-0.0.2/en_sdoh_bow/en_sdoh_bow-0.0.2 .
SOURCE=sdoh_bow_cui
#en_sdoh_bow-0.0.2

python -m spacy package \
    --force \
    $SOURCE package \
    --code lemma_tokenizer.py,sdoh_bow.py,postprocess.py
