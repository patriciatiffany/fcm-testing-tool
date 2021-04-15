# READ ME: Information about specifying models.
# Written and last modified 2021/04/07 by Patricia

In addition to constructing models within the app itself, models can be constructed manually. In some very simple or particularly complex cases, this is the preferable option.

The model is stored in json files: concepts.json and relations.json

JSON “objects” are enclosed in curly brackets ({}) and contain “key: value” pairs. Square brackets indicate arrays. For more information about json syntax, see: https://www.w3schools.com/js/js_json_syntax.asp

The different json files are outlined below:

1. concepts.json - A record of all concepts in the model.
This is an array of objects, each representing one concept in the model, like so: 
  {
    "name": "",
    "concept_id": "",
    "description": "",
    "values": {
      "min": "",
      "max": "",
      "description": ""
    },
    "group": ""
  }

 - name: the name of the concept (can be as descriptive as you’d like)
 - concept_id: the unique identifier for the concept. this is what will be used again in relations.json, and should not contain any spaces (stick with simple identifiers that contain alphanumeric characters, hyphens or underscores, and that do not start with a number)
 - description (optional): extra information about this concept
 - values (unused): object containing min/max/description information. ignore this, as it remains unused in the algorithm. 
 - group (optional): what group or category does this concept belong to?


2. relations.json - A record of all the relationships in the model.
This is an array of objects representing sets of converging/ incoming links. There should be one set for each concept in the model that is affected by other concepts. Each set is associated with a concept_id (specifying the affected concept) and other information, like so:

 {
    "concept_id": "",
    "k": "",
    "affected_by": [
      {
        "links": [
          {
            "concept_id": "",
            "direction": "",
            "weight": ""
          },
          {
            "concept_id": "",
            "direction": "",
            "weight": ""
          }
        ],
        "type": "",
        "description": ""
      }
    ]
  }


 - concept_id: the unique identifier (matching concepts.json) that represents the concept being influenced. e.g. “harvest”  

 - k: number from 0 to 1, representing the level of self-influence.

 - affected_by: an array of objects representing sets of links. (Note that so far, the model algorithm only supports models with *one* set of incoming links per concept being influenced. Each set looks like: 
      {
        "links": [
          {
            "concept_id": "",
            "direction": "",
            "weight": ""
          },
          {
            "concept_id": "",
            "direction": "",
            "weight": ""
          }
        ],
        "type": "",
        "description": ""
      }

	- links: an array of objects each representing a link. 
		- concept_id: the unique identifier (matching concepts.json) that represents the concept responsible of the influence. e.g. “warming”
		- direction: the direction of influence: “Positive” or “Negative”
		- weight: the relative strength of the link: “VL”, “L”, “M”, “H”, or “VH”

	- type: how the links in the set should be aggregated: “add” or “req”
	- description (optional): extra information about the set of links

