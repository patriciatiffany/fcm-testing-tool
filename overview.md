#### Background

To explain how the algorithm works, we'll use some terminology from the "Fuzzy" Cognitive Mapping (FCM) literature:

In a map, there are $n$ concepts connected by signed directed edges with weights $w_{ij}$ connecting some concept $C_i$ to some concept $C_j$. Typically, this connection or relationship indicates that the presence of $C_i$ influences the value of $C_j$ at the next time step or iteration, weighted by some constant $w_{ij}$. In a discrete FCM, a concept’s state value $C_j^{t}$ represents the extent to which the concept $C_j$ is present or true in the FCM system at time step or iteration $t$, defined in $[0,1]$. The state vector $\vec{C}^{t}=[C_1^t,C_2^t,…,C_n^t]$ provides a snapshot of all concept values in the system. In this body of work, the words “level”, “state”, “value”, and “state value” will be used interchangeably to refer to concept values represented by the elements of $\vec{C}^{t}$. 

In the simulation, concept values $\vec{C}^t$ are updated at each iteration. At each iteration, a concept’s value $C_j^{t+1}$ is obtained by multiplying values of all influencing concepts by their respective weights, aggregating them together, then passing the result through a transfer function (thresholding, activation, or “squashing” function) $f_j$. 

In a simple discrete FCM, a sum is used for aggregation, and the formula for calculating concept values at each timestep takes the form of $C_j^{t+1} = f_j \left(k_j C_j^{t}+ \sum_{i=1}^n C_i^{t} w_{ij} \right)$, where $k$ typically assumes a value of either be 1 or 0 depending on the type of FCM algorithm used, $w_{ij}$ is the weight of causal influence of concept $C_i$ on concept $C_j$, and $f_j$ is some function that converts the inputs into the concept node’s new state $C_j^{t+1}$.

These thresholding functions are used in order to constrain concept values within a specified range (e.g. between 0 and 1, or -1 and +1) and are often sigmoidal in nature, but they can have variations in their characteristics and parameterization (Papageorgiou and Salmeron 2014). Typically, the sigmoidal function used is the standard $f(x;\lambda,h) = \frac{1}{1+e^{-\lambda(x-h)}}$ (Knight, Lloyd, and Penn 2014). 

In order for the iterative FCM algorithm to produce valid outputs, the choice of $f$ and its implementation must be carefully justified, as small differences in initial conditions or parameters can result in drastically different outcomes (Knight, Lloyd, and Penn 2014).

<br/>

#### Algorithm used in this tool

In this modified FCM, we introduce logical conjunctions to reflect "required" (shorthand: `REQ`) relationships as well as "additive" (shorthand: `ADD`) relationships. Here, we define the algorithm such that the `REQ` inference takes the *minimum* of all influences to feed in the squashing function ($x$ above), while the `ADD` inference takes in the *mean* of the influences. 

Our algorithm then becomes $C_j^{t+1} = f_j \left(\mathrm{A}(\vec{C}^{t}) \right)$, where $\mathrm{A}$ is an aggregation function defined as $\frac{1}{m} k_j C_j^{t} + \mathrm{min}(\{C_i^{t} w_{ij}: w_{ij} \neq 0\})$ for `REQ` relation types, and $\frac{1}{m}\left(k_j C_j^{t} + \sum_{i=1}^n C_i^{t} w_{ij} \right)$ for `AND` relation types, where $m$ is the number of non-zero links influencing $C_j$, i.e. $m=\sum_{i:w_{ij} \neq 0} 1$. 

The algorithm also supports `OR` relation types, where $\mathrm{A}:= \frac{1}{m} k_j C_j^{t} + \mathrm{max}(\{C_i^{t} w_{ij}: w_{ij} \neq 0\})$, though it remains untested as of the writing of this description. If there are no influences, $\mathrm{A}(\vec{C}^{t}) = k_j C_j^{t}$.

The code for this algorithm can be found in the file `fcm.R`. This code was adapted from [this R package](https://cran.r-project.org/web/packages/fcm/index.html) by Dikopoulou and Papageorgiou.



