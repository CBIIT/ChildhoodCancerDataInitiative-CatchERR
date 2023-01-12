# ChildrensCancerDataInitiative-CatchERR
This script will take a CCDI metadata manifest file and try to blindly fix the most common errors before the validation step.

At this time, the script will catch errors for case in enumerated values, white space in values, and incomplete urls for files.

To run the script on a CDS template, run the following command in a terminal where R is installed for help.

```
Rscript --vanilla CCDI-CatchERR.R -h
```

```
Usage: CCDI-CatchERR.R [options]

CCDI-CatchERR v1.0.1

Options:
	-f CHARACTER, --file=CHARACTER
		dataset file (.xlsx, .tsv, .csv)

	-t CHARACTER, --template=CHARACTER
		dataset template file, CCDI_submission_metadata_template.xlsx

	-h, --help
		Show this help message and exit
```

To run this script on an example file, please use the following:

```
Rscript --vanilla CCDI-CatchERR.R -f test_files/b_all_fail_CCDI_Submission_Template_v1.0.2.xlsx -t test_files/b_all_fail_CCDI_Submission_Template_v1.0.2.xlsx 


Process Complete.

The output file can be found here: ChildrensCancerDataInitiative-CatchERR/test_files/
```

This will return an output file log that will tell you what changes were made, as well as a new manifest that will have the new changes applied.

```
participant
	ERROR: race property contains a value that is not recognized: white
		The value in race was changed: white ---> White
	ERROR: race property contains a value that is not recognized: Native hawaiian or other Pacific Islander
		The value in race was changed: Native hawaiian or other Pacific Islander ---> Native Hawaiian or other Pacific Islander
	PASS: gender property contains all valid values.
	ERROR: ethnicity property contains a value that is not recognized: Hispanic Or Latino
		The value in ethnicity was changed: Hispanic Or Latino ---> Hispanic or Latino
```

An example for an incomplete url can be seen in the following output:

```
The following url columns (file_url_in_cds), check to make sure the full file url is present:
----------
sequencing_file
	WARNING: The file location for the file, File_2_2.fastq, has been changed:
		s3://fakebucket14631462345/ ---> s3://fakebucket14631462345/File_2_2.fastq
	ERROR: There is an unresolvable issue with the file url for file: File_3.bam

clinical_measure_file
	WARNING: The file location for the file, extra_clinical_par_2.tsv, has been changed:
		s3://fakebucket1463142345 ---> s3://fakebucket1463142345/extra_clinical_par_2.tsv
```
