###############################################################################################################################################################################
#
#   R script to duplicate parameter values for a given functional group in Atlantis style prm files.
#
#   For example if you have an existing group called "SG" and you wish to add another groups called "SGS" and use the values for SG as a start for the parameterisations of SGS.
#
#   Note as you are adding an additional group into your file there are a number of parameters that you will need to hand edit including the diet matrix. Best to do
#   a search for parameters that have a length of 67 (or the old number of groups). 
#
#   Only change the following values:
#
###############################################################################################################################################################################

# The name of the existing prm file you wish to update.
# inputFileName = "/data/Atlantis/runFiles/trunk/SETas_model_New/VMPA_setas_biol_fishing_New_BU.prm"
inputFileName = "C:/Users/ryan.morse/Documents/GitHub/atneus_RM/at_harvest_neus_v15_DE_RM.prm"

#The name of the new prm file.
# outputFileName = "/data/Atlantis/runFiles/trunk/SETas_model_New/VMPA_setas_biol_fishing_New_Updated.prm"
outputFileName = "C:/Users/ryan.morse/Documents/GitHub/atneus_RM/at_harvest_neus_v15_DE_RM.prm"

# The groupCode of the existing group in your inputFileName prm file.
OriginalGroupName = "WHT"

# The groupCode of the new group you want to add.
AdditionalGroupName = "SWH"

###############################################################################################################################################################################

text <- readLines(inputFileName,encoding="UTF-8")
sink(outputFileName);

numLines = length(text)

for (lineIndex in 1:numLines){

    if(nchar(text[lineIndex]) > 0){
    
        match = grep(OriginalGroupName, text[lineIndex]);

        if(length(match) > 0){
       
            # Does the next line start with a number?
            startChar = substr(text[lineIndex + 1], 0, 1);
            
            result = grep("[0-9]", startChar)
                      
            if(length(result) > 0){
            
                # Double line parameter.
                write(text[lineIndex], "", append = TRUE);
                write(text[lineIndex + 1], "", append = TRUE);
               
                #Now replace the strings and print out.
                string = gsub(OriginalGroupName, AdditionalGroupName, text[lineIndex]);
                
                write(text[lineIndex + 1], "", append = TRUE);
            }else{
            
                # Single line parameter.
                write(text[lineIndex], "", append = TRUE);
               
                #Now replace the strings and print out.
                string = gsub(OriginalGroupName, AdditionalGroupName, text[lineIndex]);
                write(string, "", append = TRUE);
            }
        }
        else{
          write(text[lineIndex], "", append = TRUE);
        }
    }else{
        write(text[lineIndex], "", append = "TRUE");
    }
}

sink();
