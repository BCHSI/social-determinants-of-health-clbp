#!/usr/bin/env python
# coding: utf-8

import os
import re
from datetime import date
import pandas as pd
from sklearn.metrics import cohen_kappa_score
from itertools import product, combinations
import logging
logging.getLogger().setLevel(logging.DEBUG)
from mae_utils import parse_mae


def parse_filename(fn, pattern_filename = re.compile("(?P<note_id>\d+)(?:_(?P<annotator>[A-Za-z]+))\.xml")
                  ):
    match = pattern_filename.search(fn)
    if match:
        fn_dict = match.groupdict()
        fn_dict["note_id"] = int(fn_dict["note_id"])
    else:
        fn_dict = {"note_id":None, "annotator":None}
    return fn_dict


indir  = "data/skippy-2020-02-12-per-type"   # I edited this for my local system

tag_list = []
for dd in os.scandir(indir):
    print(dd.name)
    for ff in os.scandir(dd):
        if not ff.name.endswith("xml"):
            continue
        txt, tag_list_ = parse_mae(ff.path)

        for tag in tag_list_:
            tag["filename"] = ff.name
            tag["note_type"] = dd.name
        tag_list.extend(tag_list_)


# ## Read raw annotations

annotations_raw = pd.DataFrame(tag_list)
annotations_raw.shape


annotations_raw.note_type.value_counts()


pd.crosstab(annotations_raw.label, annotations_raw.note_type,)
#.divide(annotations_raw.note_type.value_counts(),1)


# #### parse `note_id` and expert name

annotations = pd.concat([annotations_raw, 
           annotations_raw.filename.apply(lambda fn: pd.Series(parse_filename(fn)))],1)
annotations["annotator"] = annotations.annotator.str.title().map(lambda x: {"Raf": "Rafael",
                                                                           "Mat": "Matt"}.get(x,x))
annotations = annotations.sort_values(["filename", "start"])
annotations.shape


today_iso = date.today().isoformat()
outdir = f"results/skippy-sample-2020-02-12-analysis-{today_iso}"


os.makedirs(outdir, exist_ok=True)


out_fn = f"{outdir}/annotations.csv"
print(out_fn)
annotations.to_csv(out_fn, index=False)

annotations.label.value_counts()


annotations_per_note_expert = annotations.groupby(["note_id","annotator",])["id"].count().unstack(fill_value=0)
annotations_per_note_expert.shape

# plot
pl = annotations_per_note_expert.boxplot()
pl.set_title("annotations per note");


annotations_per_note_expert.to_csv(f"{outdir}/annotation_counts_per_note_expert.csv")


# #### How many annotations have been labelled by all experts?
annotations_per_note_expert.sum()

notes_labelled_by_everyone = (annotations_per_note_expert>0).all(1)
note_ids_labelled_by_everyone = notes_labelled_by_everyone[notes_labelled_by_everyone].index.tolist()
sum(notes_labelled_by_everyone)


annotations_by_everyone = (annotations
 .value_counts(["note_id", "label", "type",  "annotator",])
 .unstack(fill_value=0)
 .loc[note_ids_labelled_by_everyone]
    )

annotations_by_everyone.    to_csv(f"{outdir}/annotation_counts_per_note_expert_label.csv")


# ## Evaluate Inter-rater agreement

experts = annotations_by_everyone.columns

kappas = {}
for e1, e2 in combinations(experts, 2):
    kappa_ = cohen_kappa_score(annotations_by_everyone[e1],
                               annotations_by_everyone[e2])
    kappas[(e1,e2)] = kappa_


# ## Calculate agreement per label

experts = annotations_by_everyone.columns

number_of_annotations_per_label = {}
kappas_per_label = {}
for _, row  in unique_labels.iterrows():
    kappas_ = {}
    number_of_annotations_per_label[tuple(row.tolist())] = len(annotations_by_everyone.loc[(slice(None),)+tuple(row.tolist())])
    
    for e1, e2 in combinations(experts, 2):
        col1 = annotations_by_everyone.loc[(slice(None),)+tuple(row.tolist()), (e1)]
        col2 = annotations_by_everyone.loc[(slice(None),)+tuple(row.tolist()), (e2)]
        number_of_annotations_per_label[tuple(row.tolist())] = len(col1)
        if len(col1)<2:
            continue
        kappa_ = cohen_kappa_score(col1, col2)
        kappas_[(e1,e2)] = kappa_
#         print('_, row', _, row)
#         print("e1, e2: ", e1, e2)   # Added this to troubleshoot
#         print("col1, col2: ", col1, col2)   # Added this to troubleshoot
    kappas_per_label[tuple(row.tolist())] = kappas_
    
number_of_annotations_per_label = pd.Series(number_of_annotations_per_label)
kappas_per_label = pd.DataFrame(kappas_per_label).T
kappas_per_label.shape

# ## Average agreement for all labels per expert pair
# this value is not weighted by the label frequency
kappas_per_label[number_of_annotations_per_label>2].mean().unstack().style.background_gradient(axis=None).highlight_null('grey')


# ## Average agreement for all expert pairs per label
kappas_per_label[number_of_annotations_per_label>2].mean(1).sort_values()
