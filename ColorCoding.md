# Proposal for Color Coding Matches

## Background Color: Match type

The background color of a cell / chunk number depends on the type of the match.

If no match is found, we use a grey background.

For a regression match it is determined by reg_num_type.

We will use 3 variants of red (coef, parenthesis, stat) for regression matches.

For a non regression match, the color is determined by the `cmd` of the matching chunk. We use all non-red, non-grey colors.  
