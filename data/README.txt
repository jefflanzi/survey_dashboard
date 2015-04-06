Configuration Instructions

Data Folder Requires 3 files:

1. survey_completes.csv
  Export results to application >
    Format = CSV
    Completion State = Completed Responses Only 
    Headings = Question Code
    Responses = Answer Codes
        Convert Y to 1
        Convert N to 0
2. meta_data.txt
  Export > Tab-seperated-values format (*.txt)
3. qtypes.csv
  This file must be created manually
  $qid = qid's matching limesurvey design, can be queried as: meta[meta$class == "Q", "name"]
  $qtype = selected manually, this influences the style of table/graph chosen
		current support types:
			free_text
			single_choice
			multiple_choice
			likert_avg
			likert_sum
			nps
  $shortname = manually designated shortname to meaningfully identify the question. Can be anything
  $segment = 1/0; binary flag indicating whether this question should be used for segmentation (only L & M qtypes supported)


Features in development:
Support for incomplete surveys
Support for Y/N character vs 1/0 answer codes
Guided creation of qtypes.csv
Positive Likert Value importing