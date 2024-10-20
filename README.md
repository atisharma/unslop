## The data
This is a set of tools to generate synthetic datasets, but with close adherence to high-quality primary sources. The standard setup is to generate playthroughs in the style of interactive fiction or adventure games, but it's simple to apply your own templates for your own needs.

## Generation and filtering
The `adventure` workflow/template works as follows.

- The original sources are chunked and filtered to remove formatting and non-prose text.
- Those and the resulting generated conversations are then rated for quality (automatic judgement) and assigned one of awful, poor, mediocre, good, excellent and outstanding. Anything below good in either category is discarded.
- A slop score is also calculated, which is a heuristic involving the frequency of [slop words](https://github.com/sam-paech/antislop-sampler), and data with too high a score are disregarded.

## Safety
The `adventure` template includes a BBFC-style rating system which allows fine-grained filtering based on rating (if you trust your model's judgement).
