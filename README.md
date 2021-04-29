# fcm-testing-tool

The Qualitative Systems Modeller (fcm-testing-tool) is an application for modelling qualitative, directed graphs. A directed graph is composed of 'nodes' and 'edges', where nodes are concepts that are being modeled and edges specify relationships between concepts .

The direction of each edge specifies the relationship between influencing and affected concepts, with edge weights specify the strength ("very low" to "very high") and directionality ("positive" or "negative") of the influence. A positive sign for an edge weight means that an increase in the influencing (or "causal") concept generally causes an increase in the affected concept.

This tool allows the user to run a computer simulation that uses the qualitative, directed graph to project possible future states using a modified "Fuzzy" Cognitive Mapping (FCM) algorithm. The user can also tweak algorithm settings as well as test out different scenarios, whereby one or more concepts are "clamped" or fixed at certain values throughout the simulation.

The FCM code and the user interface in this repository was adapted from an existing ['fcm' R package](https://cran.r-project.org/web/packages/fcm/index.html) by Dikopoulou and Papageorgiou, and the ['Logic Laboratory'](https://github.com/gregorbj/FSDM) by Brian Gregor.

Parts of the code that are relevant for analysis can be found in the helper.R file, as well as fcm.R. 
