# READ ME: Using the app.
# Written and last modified 2021/04/08 by Patricia

0) Enter User Info
This information is used each time the model is saved, when timestamp and author information is stored in status.json (models$status). You can take a look at what this looks like in 1) Model Setup > Upload model > Model save history

Checking “Keep me anonymous” enters “Anonymous” instead of an author name.


1) Model Setup

Here is where you upload, edit, or create a new model. In particular, you can edit the concepts and the relationships associated with each model in the “Edit Concepts” and “Edit Relationships” tabs. In the “Edit Weight Values” tab, you can also change how qualitative weights (“VL” very low to “VH” very high) maps onto quantitative values between 0 and 1. 

The “Save model” button on each edit page will save all edits on the model thus far (including any changes on other tabs). The model is saved locally in the models/[modelName]/ folder.

You can see what your model looks like visually with the “Visualize Model” tab. Note that this will produce an error if there no concepts exist in the model.

Note: the creation of a new model requires the presence of templates in the models/ folder (!) do not touch this


2) FCM Exploration

Here is where you can run the FCM algorithm on your model and see what the outcome of the computer simulation looks like. You can run the model with different configurations, using the sliders on the left hand side to test out different parameter combinations. 

Parameters include: initial values at the start of the simulation, h (the threshold/ x offset), and lambda (steepness of the curve). You may also select your choice of thresholding function (logistic sigmoid, tanh sigmoid, or a linear function), as well as for how many timesteps to run the simulation. (Note: there is no lambda value associated with a linear thresholding function)

To test out scenarios, you can add constraints in a sub-tab in the sidebar, to constrain certain concepts at specific values throughout the simulation. 

When you have selected your parameters and constraints of choice, click the “Run simulation” button. 

Once the run is complete, if you’d like to save this scenario for future use (i.e. to compare with other runs in “3) Compare Scenario Results”), click on the “Add to scenario comparisons” button. The scenario name field auto-populates with a name that contains information on the constraints applied, the thresholding function used (exp or tanh or linear), and the h (h), lambda (L), and initial values (i) used. 

On the “Run Results” tab, you can run different parameter combinations one at a time. A few other tabs exist for your convenience / specific use cases:

“Configure Multiple Runs” tab:
Use this tab when you want to test different values of a single parameter (h, lambda, or initial simulation values), e.g. in sensitivity analyses. If there are any conflicts, this parameter “sweep” will override values selected in the sidebar. Type the values you’d like to run in the “Values” textbox as numbers, separated by commas.

Like in the main tab, the “Add runs to scenario comparison view” will allow you to access these runs in the the scenario comparison view (“3) Compare Scenario Results”)

“Configure Multiple Constraints” tab:
Use this tab to easily configure high/low scenarios for different concepts. Choose concepts for which you would like to test high/low scenarios. 

For each concept chosen, this will configure 3 simulations:
1. a simulation in which that concept is constrained at 1
2. a simulation in which that concept is constrained at 0 (or -1, depending on the thresholding function of choice, i.e. for tanh)
3. a baseline scenario (a run with the same parameters, but no constraints).


“Test sensitivity to weights” tab:
Use this tab to test out the model simulation using randomly generated weights and see whether there is a large spread in results. This is especially useful if you are not sure about the accuracy of validity of the quantitative weights associated with each of the relationships. Try testing this with different sets of parameters, as this might affect how sensitive the results are to the quantitative relationship weights in the model.

This “monte carlo simulation” runs 100 models with identical structure and relationship directions (positive/ negative), but with randomly generated weights, and displays the results. This process may take a minute or two, so clicking only once and waiting is advised.

You may use the slider to a highlight a specific run in the monte carlo simulation and view what the adjacency matrix (weights) look like for that particular model run.

3) Compare Scenario Results
Here is where you can view the results of multiple runs. This feature works best when you have a number of scenarios (sets of constraints) you would like to compare, along with its associated baseline (a simulation run with the same parameters, but no constraints).

Any scenarios saved from other tabs will show up in the left hand side, under the section “Compare these scenarios”. Select the scenarios you would like to compare here, making sure that a corresponding baseline scenario is also selected.

(On the right hand side, a reset button allows you to delete all scenarios available for comparison, and a load scenarios function allows you to load previously saved scenarios to view the results without re-running the model. This looks for .Rmd files in the /models/[ModelName]/scenarios folder. Caution: Be careful with this loading scenarios function— this will override all scenarios loaded/ saved for the comparison view.)

The “Launch scenario comparison view” button will display plots below. Some include references to a baseline scenario and will only work if the corresponding baseline results are also loaded.

At the bottom of the page, you will find a button to save the plots contents (results of calculations) as a data frame, in an .Rmd file in the /models/[ModelName]/analysis folder. This allows you to use this for your own analysis. 

You can read the scenario data files directly into R, with the readRDS() function. For example: 
df <- readRDS(“~/link/to/file.Rmd”)



