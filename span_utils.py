import pandas as pd
import logging

def _tok_to_dict(ent, snippet=None):
    result = {"text": ent.text,
              "start": ent.start_char, "end": ent.end_char,
              "start_tok": ent.start, "end_tok": ent.end,
              "label": ent.label_}

    for ext in dir(ent._):
        if ext not in {"has", "set", "get"}:
            val = getattr(ent._, ext)
            if val is not None:
                result[ext] = val

    if hasattr(ent._, "type"):
        result["type"] = ent._.type

    if snippet:
        doc = ent.doc
        if isinstance(snippet, int):
            result["snippet"] = doc[max(0, ent.start-snippet):min(ent.end+snippet, len(doc))].text.strip()
        elif isinstance(snippet, list):
            for sn in snippet:
                result[f"snippet_{sn}"] = doc[max(0, ent.start-sn):min(ent.end+sn, len(doc))].text.strip()
    return result

def get_ent_dict(ents, snippet = None):
    return [_tok_to_dict(ent, snippet=snippet) for ent in ents]

def get_ent_df(ents, snippet = None):
    ents = get_ent_dict(ents, snippet = snippet)
    return pd.DataFrame(ents)


def join_example_ents(example, cols=["label", "text"],
                      cols_ref=[], cols_pred=[], join="left"):
    """
    Input:
    - example: spacy Example object with the reference (ground truth) and prediciton
    - cols:    columns to keep
    - join:    join type (inner, left, right, outer)
    """
    refs = get_ent_df(example.reference.ents)
    preds = get_ent_df(example.predicted.ents)
    return join_on_span_overlaps(refs, preds, cols=cols,
                                 cols_left=cols_ref,
                                 cols_right=cols_pred,
                                 join=join,
                                 suffixes = ("_ref", "_pred"))


def add_span(df):
    """add a "span" column of `pd.Interval` type based on "start" and "end" columns
    """
    df.loc[:,"span"] = df.apply(lambda row: pd.Interval(row["start"], row["end"], closed="both"),1)
    df.loc[:,"start"] = df["start"].astype(pd.Int64Dtype())
    df.loc[:,"end"]   = df["end"].astype(pd.Int64Dtype())


def join_on_span_overlaps(left, right, cols=["label"],
                 cols_left = [],
                 cols_right = [],
                 suffixes=("_left", "_right"),
                 join = "inner",
                ):
    """Join two tables based on overlaps of intervals defined in "start" and "end" columns
    """
    if len(left) == 0:
        logging.debug("left input has no rows")
        return pd.DataFrame([])
    elif len(right) == 0:
        logging.debug("right input has no rows")
        return pd.DataFrame([])

    add_span(left)
    add_span(right)
    index_cols = ["start", "end"] + cols


    for cc in cols:
        if cc in cols_right:
            cols_right.remove(cc)
        if cc in cols_left:
            cols_left.remove(cc)

    common_cols = set(left.columns) & set(right.columns)

    cols_set_right = ({None} | set(cols_right) )
    cols_set_left = ({None} | set(cols_left) )


    right_ = (
        right
        .set_index(index_cols + cols_right,
                   append=len(set(right.index.names) - {None})>0)
         )
    cols_right_suffixes = [(col + suffixes[1] if col in common_cols else col) for col in right_.index.names]

    right_ = right_.rename(columns=dict(zip(index_cols + cols_right, cols_right_suffixes)))

    xoverlap = left.set_index(index_cols + cols_left,
                              append=len(set(left.index.names) - {None})>0
                             ).span.map(
        lambda y: (right_.span.map(lambda x: x.overlaps(y)))
                    )

    cols_left_suffixes = [(col + suffixes[0] if col not in cols_set_left else col)
                          for col in xoverlap.index.names]
    xoverlap = pd.concat(xoverlap.to_dict(),
                         names= cols_left_suffixes + cols_right_suffixes
                        )

    xoverlap = xoverlap[xoverlap]
    xoverlap = (xoverlap
                .reset_index()
                .drop(columns=["span"])
                #.rename(columns=dict(zip(index_cols + cols_left, cols_left_suffixes)))
               )

    # import ipdb; ipdb.set_trace()
    key_start = "start" + suffixes[1]
    key_end = "end" + suffixes[1]
    xoverlap[key_start] = xoverlap[key_start].astype(pd.Int64Dtype())
    xoverlap[key_end]   = xoverlap[key_end].astype(pd.Int64Dtype())


    if join in {"left", "outer"}:
        left_ = left.rename(columns=dict(zip(common_cols,
                            [col + suffixes[0] for col in common_cols])))
        xoverlap = xoverlap.rename(columns=dict(zip(common_cols,
                            [col + suffixes[1] for col in common_cols])))
        xoverlap = pd.merge(left_,
                            xoverlap, how="left",
                 on=cols_left_suffixes
                )

    if join in { "right", "outer"}:
        right_ = right_.reset_index().rename(columns=dict(zip(common_cols,
                                                              [col + suffixes[1] for col in common_cols])))
        na_mask = xoverlap[cols_right_suffixes].isnull().any(1)
        xoverlap_na = xoverlap[na_mask]

        xoverlap = pd.merge(
                            xoverlap[~na_mask],
                            right_.reset_index(),
                            how=join,
                 on=cols_right_suffixes,
                 suffixes=suffixes,
                )
        xoverlap = pd.concat([xoverlap, xoverlap_na])
    return xoverlap
