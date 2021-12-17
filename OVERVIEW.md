_WARNING: This package is experimental and a proof of concept what could be done with **BiocParallel** and the future framework._

<%
## Reuse the future vignette
md <- R.rsp::rstring(file="vignettes/FutureParam.md.rsp", postprocess=FALSE)

## Drop the header
md <- unlist(strsplit(md, split="\n", fixed=TRUE))
md <- md[-seq_len(grep("^## ", md)[1]-1)]

## Output
cat(md, sep="\n")
%>
