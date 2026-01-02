# Mills

Currently mills are always run on demand with the full calculations. This is
an interim approach which is not efficient. One of the things to design and
implement is caching or saving them in some way.

## Informed popularity

Informed popularity is based on various explore-exploit schemes that canonically
use Click-Through Rate (CTR) as the basis for recommendations. We call it
"informed" because we can compute the popularity only for the segment to which the
case (user) belongs. The case features used for this segmentation are determined
by the governor.

CTR needs decisions (past recommendations) to be available. If they're not, we
want to use raw popularity - just interactions.

This type of mill is useful where we have user interactions (and ideally
recommender decisions) but not that much meaningful features and metadata for
users and items.

For inspiration, see the Chapters 1, 3 and 6 of Agarwal and Chen's "Statistical
Methods for Recommender Systems" book.

## Nearest options

Nearest options are for recommending either items similar to what the user
already interacted with, or items interacted with by similar users (classic
Collaborative Filtering). The nearest-option-type-mill function automatically
does Collaborative Filtering for users with no interactions.

Nearest options is best when items and/or users have meaningful metadata and can
tolerate less interactions being available overall.

For inspiration, see Chapters 7 and 8 of Kim Falk's "Practical Recommender
Systems" and Chapter 2 of Agarwal and Chen's "Statistical Methods for Recommender
Systems" book.
