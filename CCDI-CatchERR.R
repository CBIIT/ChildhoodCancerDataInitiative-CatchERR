#!/usr/bin/env Rscript

#Childhood Cancer Data Initiative - CatchERR.R

#This script will take a CCDI metadata manifest file and try to blindly fix the most common errors before the validation step.

##################
#
# USAGE
#
##################

#Run the following command in a terminal where R is installed for help.

#Rscript --vanilla CCDI-CatchERR.R --help


##################
#
# Env. Setup
#
##################

#List of needed packages
list_of_packages=c("readr","dplyr","tidyr","openxlsx","stringi","readxl","janitor","optparse","tools")

#Based on the packages that are present, install ones that are required.
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
suppressMessages(if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org"))

#Load libraries.
suppressMessages(library(readr,verbose = F))
suppressMessages(library(dplyr,verbose = F))
suppressMessages(library(tidyr,verbose = F))
suppressMessages(library(readxl,verbose = F))
suppressMessages(library(openxlsx, verbose = F))
suppressMessages(library(stringi,verbose = F))
suppressMessages(library(janitor,verbose = F))
suppressMessages(library(optparse,verbose = F))
suppressMessages(library(tools,verbose = F))


#remove objects that are no longer used.
rm(list_of_packages)
rm(new.packages)


##################
#
# Arg parse
#
##################

#Option list for arg parse
option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="dataset file (.xlsx, .tsv, .csv)", metavar="character"),
  make_option(c("-t", "--template"), type="character", default=NULL, 
              help="dataset template file, CCDI_submission_metadata_template.xlsx", metavar="character")
)

#create list of options and values for file input
opt_parser = OptionParser(option_list=option_list, description = "\nCCDI-CatchERR v1.1.0")
opt = parse_args(opt_parser)

#If no options are presented, return --help, stop and print the following message.
if (is.null(opt$file)&is.null(opt$template)){
  print_help(opt_parser)
  cat("Please supply both the input file (-f) and template file (-t), CCDI_submission_metadata_template.xlsx.\n\n")
  suppressMessages(stop(call.=FALSE))
}


#Data file pathway
file_path=file_path_as_absolute(opt$file)

#Template file pathway
template_path=file_path_as_absolute(opt$template)

###########
#
# File name rework
#
###########

#Rework the file path to obtain a file extension.
file_name=stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][2])
ext=tolower(stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][1]))
path=paste(dirname(file_path),"/",sep = "")

#Output file name based on input file name and date/time stamped.
output_file=paste(file_name,
                  "_CatchERR",
                  stri_replace_all_fixed(
                    str = Sys.Date(),
                    pattern = "-",
                    replacement = ""),
                  sep="")

##############
#
# Pull Dictionary Page to create node pulls
#
##############

#Read in Dictionary page to obtain the required properties.
df_dict=suppressMessages(read.xlsx(xlsxFile = template_path,sheet = "Dictionary"))
df_dict=remove_empty(df_dict,c('rows','cols'))

#Look for all entries that have a value
all_properties=unique(df_dict$Property)[grep(pattern = '.',unique(df_dict$Property))]
#Remove all entries that are all spaces
all_properties=all_properties[!grepl(pattern = " ",x = all_properties)]
#Pull out required property groups
required_property_groups=unique(df_dict$Required[!is.na(df_dict$Required)])
required_properties=df_dict$Property[!is.na(df_dict$Required)]
#Pull out nodes to read in respective tabs
dict_nodes=unique(df_dict$Node)


################
#
# Read in TaVS page to create value checks
#
################

#Read in Terms and Value sets page to obtain the required value set names.
df_tavs=suppressMessages(read.xlsx(xlsxFile = template_path, sheet = "Terms and Value Sets"))
df_tavs=remove_empty(df_tavs,c('rows','cols'))

#Pull out the positions where the value set names are located
VSN=unique(df_tavs$Value.Set.Name)
VSN=VSN[!is.na(VSN)]

df_all_terms=list()

#Pull the list of values for each controlled vocabulary property.
for (VSN_indv in VSN){
  df_all_terms[[VSN_indv]] = list(filter(df_tavs,Value.Set.Name==VSN_indv)["Term"][[1]])
}


##############
#
# Read in each tab and apply to a data frame list
#
##############

# A bank of NA terms to make sure NAs are brought in correctly
NA_bank=c("NA","na","N/A","n/a")

#Establish the list
workbook_list=list()
incomplete_node=c()

#create a list of all node pages with data
for (node in dict_nodes){
  #read the sheet
  df=suppressMessages(read_xlsx(path = file_path, trim_ws = TRUE, na=NA_bank, sheet = node, guess_max = 1000000, col_types = "text"))
  #df=readWorkbook(xlsxFile = file_path,sheet = node, na.strings = NA_bank)
  #create an emptier version that removes the type and makes everything a character
  df_empty_test=df%>%
    select(-type)%>%
    mutate(across(everything(), as.character))
  #remove empty rows and columns
  df_empty_test=remove_empty(df_empty_test,c("rows","cols"))
  
  #if there are at least one row in the resulting data frame, add it
  if (dim(df_empty_test)[1]>0){
    #if the only columns in the resulting data frame are only linking properties (node.node_id), do not add it.
    if (any(!grepl(pattern = "\\.",x = colnames(df_empty_test)))){
      #add the data frame to the workbook
      workbook_list=append(x = workbook_list,values = list(df))
      names(workbook_list)[length(workbook_list)]<-node
    }else{
      incomplete_node=c(incomplete_node,node)
    }
  }
}

nodes_present=names(workbook_list)


#############
#
# Data frame manipulation
#
#############


#Start write out for log file
sink(paste(path,output_file,".txt",sep = ""))

if (!is.null(incomplete_node)){
  cat("\n\tWARNING: The following node(s), ",paste(incomplete_node, collapse = ", ", sep = ),", did not contain any data except a linking value and type.\n" ,sep = "")
}


##################
#
# Terms and Value sets checks
#
##################

cat("\n\nThe following columns have controlled vocabulary on the 'Terms and Value Sets' page of the template file. If the values present do not match, they will noted and in some cases the values will be replaced:\n----------")

#Enumerated Array properties
enum_arrays=c('therapeutic_agents',"treatment_type","study_data_types","morphology","primary_site","race")

for (node in nodes_present){
  cat("\n",node,"\n",sep = "")
  #initialize data frames and properties for tests
  df=workbook_list[node][[1]]
  properties=colnames(df)
 
  for (property in properties){
    if (property %in% names(df_all_terms)){
      #for enum_array properties
      if (property %in% enum_arrays){
        
        #Sort each array before checks
        for (array_value_pos in 1:length(df[property][[1]])){
          array_value=df[property][[1]][array_value_pos]
          if (grepl(pattern = ";", array_value)){
            
            #alphabetize array
            array_value=paste(sort(trimws(unique(stri_split_fixed(str = array_value, pattern = ";")[[1]]))), collapse = ";")
            df[property][[1]][array_value_pos]=array_value
          }
        }
        
        #Set up to find values that are not in the expected permissible value (PV) list
        unique_values=unique(df[property][[1]])
        unique_values=unique(trimws(unlist(stri_split_fixed(str = unique_values,pattern = ";"))))
        unique_values=unique_values[!is.na(unique_values)]
        if (length(unique_values)>0){
          if (!all(unique_values%in%df_all_terms[property][[1]][[1]])){
            for (x in 1:length(unique_values)){
              check_value=unique_values[x]
              if (!is.na(check_value)){
                if (!as.character(check_value)%in%df_all_terms[property][[1]][[1]]){
                  cat(paste("\tERROR: ",property," property contains a value that is not recognized: ", check_value,"\n",sep = ""))
                  #NEW ADDITION to push the most correct value into the proper case
                  if (tolower(as.character(check_value))%in%tolower(df_all_terms[property][[1]][[1]])){
                    
                    #determine the correct PV
                    pv_pos=grep(pattern = TRUE, x = tolower(df_all_terms[property][[1]][[1]])%in%tolower(as.character(check_value)))
                    
                    #find all the value positions for the property with wrong value
                    value_positions=grep(pattern = check_value, x = df[property][[1]])
                    replacement_value=df_all_terms[property][[1]][[1]][pv_pos]
                    #create a filter to remove positions that are erroneously selected and either find only values that are the whole string or the string in part of an array list.
                    for (value_pos in value_positions){
                      previous_value=df[value_pos,property][[1]]
                      if (nchar(previous_value)!=nchar(replacement_value)){
                        array_pattern=c(paste(replacement_value,";",sep = ""),paste(";",replacement_value,sep = ""))
                        array_pos=grep(pattern = tolower(paste(array_pattern,collapse = "|")), x = tolower(previous_value))
                        if (length(array_pos)==0){
                          value_positions=value_positions[!(value_positions %in% value_pos)]
                        }
                      }
                    }
                    
                    #create dataframe to capture values changed as to not overload with lines
                    prev_repl_df=tibble(previous_value_col=NA, replacement_value_col=NA)
                    prev_repl_df_add=tibble(previous_value_col=NA, replacement_value_col=NA)
                    prev_repl_df=prev_repl_df[0,]
                    
                    #for each position, change the value in the array.
                    for (value_pos in value_positions){
                      previous_value=df[value_pos,property][[1]]
                      replacement_string=stri_replace_all_fixed(str =previous_value, pattern = as.character(check_value), replacement = replacement_value)
                      df[value_pos,property]<-replacement_string
                      
                      prev_repl_df_add$previous_value_col=previous_value
                      prev_repl_df_add$replacement_value_col=replacement_string
                      
                      prev_repl_df=unique(rbind(prev_repl_df,prev_repl_df_add))
                    }
                    
                    
                    for ( prdf in 1:dim(prev_repl_df)[1]){
                      cat(paste("\t\tThe value in ",property,", was changed: ", prev_repl_df$previous_value_col[prdf]," ---> ",prev_repl_df$replacement_value_col[prdf],"\n",sep = ""))
                    }
                  }
                }
              }
            }
          }else{
            cat(paste("\tPASS:",property,"property contains all valid values.\n"))
          }
        }
      }
      #for non-enum_array properties
      else{
        unique_values=unique(df[property][[1]])
        unique_values=unique_values[!is.na(unique_values)]
        if (length(unique_values)>0){
          if (!all(unique_values%in%df_all_terms[property][[1]][[1]])){
            for (x in 1:length(unique_values)){
              check_value=unique_values[x]
              if (!is.na(check_value)){
                if (!as.character(check_value)%in%df_all_terms[property][[1]][[1]]){
                  cat(paste("\tERROR: ",property," property contains a value that is not recognized: ", check_value,"\n",sep = ""))
                  #NEW ADDITION to push the most correct value into the proper case
                  if (tolower(as.character(check_value))%in%tolower(df_all_terms[property][[1]][[1]])){
                    pv_pos=grep(pattern = TRUE, x = tolower(df_all_terms[property][[1]][[1]])%in%tolower(as.character(check_value)))
                    value_pos=grep(pattern = TRUE, x = df[property][[1]]%in%as.character(check_value))
                    df[value_pos,property]<-df_all_terms[property][[1]][[1]][pv_pos]
                    cat(paste("\t\tThe value in ",property," was changed: ", check_value," ---> ",df_all_terms[property][[1]][[1]][pv_pos],"\n",sep = ""))
                  }
                }
              }
            }
          }else{
            cat(paste("\tPASS:",property,"property contains all valid values.\n"))
          }
        }
      }
    }
  }
  workbook_list[node][[1]]=df
}
      
                 
##############
#
# Check and replace for non-UTF-8 characters
#
##############

cat("\n\nCertain characters do not handle being transformed into certain file types, due to this, the following characters were changed.\n----------\n")

#initialize table and then populate the key value pairs for the value that is present and what it needs to be converted to.
df_translations=tibble(value="",translation="")[0,]

#Expression to expand further pairs, copy the following line and add the new value pair.
df_translations=rbind(df_translations,tibble(value="™",translation="(TM)"))


#Grep through column and rows looking for values to change.
for (node in nodes_present){
  df_translate=workbook_list[node][[1]]
  cat(paste("\n",node,"\n",sep = ""))
  for (value in 1:dim(df_translations)[1]){
    df_value=df_translations[value,"value"]
    df_transvalue=df_translations[value,"translation"]
    colnums=grep(pattern = df_value, df_translate)
    for (colnum in colnums){
      rownums=grep(pattern = df_value, df_translate[,colnum][[1]])
      for (rownum in rownums){
        df_translate[rownum,colnum]=stri_replace_all_fixed(str = df_translate[rownum,colnum], pattern = df_value, replacement = df_transvalue)
      }
      cat(paste("\tWARNING: ",colnames(df_translate[,colnum])," contains value(s) that were changed to ensure UTF-8.\n\t\tThe value that was changed: ",df_value," ---> ",df_transvalue,"\n", sep = ""))
      workbook_list[node][[1]]=df_translate
    }
  }
}
   

##############
#
# ACL pattern check
#
##############
cat("\n\nThe value for ACL will be check to determine it follows the required structure, ['.*'].\n----------\n")

acl_check=unique(workbook_list['study'][[1]]$acl)

if (length(acl_check)>1){
  cat("ERROR: There is more than one ACL associated with this study and workbook. Please only submit one ACL and corresponding data to a workbook.\n")
}else if(length(acl_check)==1){
  if (is.na(acl_check)){
    cat("ERROR: Please submit an ACL value to the 'study_admin.acl' property.\n")
  }else if (!is.na(acl_check)){
    acl_test=grepl(pattern = "\\[\\'.*\\'\\]" , x= acl_check)
    if (!acl_test){
      acl_fix=paste("['",acl_check,"']", sep="")
      cat("The following ACL does not match the required structure, it will be changed:\n\t\t", acl_check, " ---> ", acl_fix,"\n",sep = "")
      workbook_list['study'][[1]]$acl=acl_fix
    }else if (acl_test){
      cat("The following ACL matches the required structure:\n\t\t", acl_check,"\n",sep = "")
    }
  }
}else{
  cat("ERROR: Something is wrong with the ACL value submitted in the study_admin.acl property.\n")
}


##############
#
# Fix URL paths
#
##############

cat("\n\nCheck the following url columns (file_url_in_cds), to make sure the full file url is present and fix entries that are not:\n----------")

for (node in nodes_present){
  
  #initialize data frames and properties for tests
  df=workbook_list[node][[1]]
  properties=colnames(df)
  
  if ("file_url_in_cds" %in% properties){
    cat("\n",node,sep = "")
    
    #look at all the urls in the data frame
    node_urls=df %>%
      select(file_url_in_cds)%>%
      filter(!is.na(file_url_in_cds))%>%
      separate(file_url_in_cds,c("s3","blank","base_urls","everything_else"),sep = "/", extra = "merge", fill= "right")%>%
      select(base_urls)
    
    #pull out all the unique base bucket urls
    node_urls=unique(node_urls$base_urls)
    node_urls=node_urls[!is.na(node_urls)]
    
    #blank list of bad url_locations
    bad_url_locs=c()
    
    #for each possible bucket based on the base urls in file_url_in_cds
    #go through and see if the values for the url can be filled in based on file_name and size
    for (node_url in node_urls){
      #pull bucket metadata
      
      metadata_files=suppressMessages(suppressWarnings(system(command = paste("aws s3 ls --recursive s3://", node_url,"/",sep = ""),intern = TRUE)))
      
      #fix bucket metadata to have fixed delimiters of one space
      while (any(grepl(pattern = "  ",x = metadata_files))==TRUE){
        metadata_files=stri_replace_all_fixed(str = metadata_files,pattern = "  ",replacement = " ")
      }
      
      #Break bucket string into a data frame and clean up
      bucket_metadata=data.frame(all_metadata=metadata_files)
      bucket_metadata=separate(bucket_metadata, all_metadata, into = c("date","time","file_size","file_path"),sep = " ", extra = "merge")%>%
        select(-date, -time)%>%
        mutate(file_path=paste("s3://",node_url,"/",file_path,sep = ""), file_name=basename(file_path))
      
      #find bad url locs based on the full file path and whether it can be found in the url manifest.
      
      for (bucket_loc in 1:dim(df)[1]){
        bucket_url=df$file_url_in_cds[bucket_loc]
        
        url_search_response=bucket_url %in% bucket_metadata$file_path
        
        if (!url_search_response){
          bad_url_locs=c(bad_url_locs, bucket_loc)
        }
        
      }
      
      #Go through each bad location and determine if the correct url location can be determined on file_name and file_size.
      for (bad_url_loc in bad_url_locs){
        #find file name and file size
        file_name_find=df$file_name[bad_url_loc]
        file_size_find=df$file_size[bad_url_loc]
        
        #filter the df to see if there is exactly one value that matches
        filtered_df=bucket_metadata[grep(pattern = TRUE, x = bucket_metadata$file_name %in% file_name_find),]
        filtered_df=filtered_df[grep(pattern = TRUE, x = filtered_df$file_size %in% file_size_find),]
        
        if (dim(filtered_df)[1]==1){
          #output of url change
          cat(paste("\n\tWARNING: The file location for the file, ", file_name_find,", has been changed:\n\t\t", df$file_url_in_cds[bad_url_loc], " ---> ", filtered_df$file_path,sep = ""))
          
          #apply change to df
          df$file_url_in_cds[bad_url_loc]=filtered_df$file_path
          
          #remove the position from the list
          bad_url_locs=bad_url_locs[!bad_url_locs %in% bad_url_loc]
          
        }else{
          cat(paste("\n\tERROR: There is an unresolvable issue with the file url for file: ",file_name_find,sep = ""))
        }
      }
    }
    cat("\n")
    workbook_list[node][[1]]=df
  }
}

#close log file write out
sink()


###############
#
# Assign guids to files
#
###############

cat("\nThe file based nodes will now have a guid assigned to each unique file.\n")


#Function to determine if operating system is OS is mac or linux, to run the UUID generation.
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

for (node in nodes_present){
  cat("\nguid creation for the following node: ", node,"\n", sep = "")
  df=workbook_list[node][[1]]
  properties=colnames(df)
  if ("file_url_in_cds" %in% properties & "file_name" %in% properties & "file_size" %in% properties & "md5sum" %in% properties & "dcf_indexd_guid" %in% properties){
    df_index=df%>%
      select(file_url_in_cds, file_name, file_size, md5sum, dcf_indexd_guid)%>%
      mutate(guid=dcf_indexd_guid)
    df_index=unique(df_index)
    #For each unique file, apply a uuid to the guid column. There is logic to handle this in both OSx and Linux, as the UUID call is different from R to the console.
    pb=txtProgressBar(min=0,max=dim(df_index)[1],style = 3)
    
    for (x in 1:dim(df_index)[1]){
      setTxtProgressBar(pb,x)
      if (is.na(df_index$guid[x])){
        if (get_os()=="osx"){
          uuid=tolower(system(command = "uuidgen", intern = T))
        }else{
          uuid=system(command = "uuid", intern = T)
        }
        #Take the uuids in the guid column and paste on the 'dg.4DCF/' prefix to create guids for all the files.
        df_index$guid[x]=paste("dg.4DFC/",uuid,sep = "")
      }
    }
    
    df=suppressMessages(left_join(df,df_index))
    
    df=df%>%
      mutate(dcf_indexd_guid=guid)%>%
      select(-guid)
    
    workbook_list[node][[1]] = df
  }
}


###############
#
# Write out
#
###############

#Write out file

wb=openxlsx::loadWorkbook(file = file_path)

cat("\n\nWriting out the CatchERR file.\n")

#progress bar
pb=txtProgressBar(min=0,max=length(nodes_present),style = 3)
x=0

for (node in nodes_present){
  x=x+1
  setTxtProgressBar(pb,x)
  df=workbook_list[node][[1]]
  openxlsx::deleteData(wb, sheet = node,rows = 1:(dim(df)[1]+1),cols=1:(dim(df)[2]+1),gridExpand = TRUE)
  openxlsx::writeData(wb=wb, sheet=node, df)
  openxlsx::saveWorkbook(wb = wb,file = paste(path,output_file,".xlsx",sep = ""), overwrite = T)
}



cat(paste("\n\nProcess Complete.\n\nThe output file can be found here: ",path,"\n\n",sep = "")) 
