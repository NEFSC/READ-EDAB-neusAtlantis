load_atlantis_catch <- function(file_path) {
  # 1. Read all lines to handle the header separately
  all_lines <- readLines(file_path)
  
  # 2. Extract Column Names from Metadata
  # We look for lines containing "COLUMN" and ".name"
  name_lines <- all_lines[grep("## COLUMN.*\\.name", all_lines)]
  
  # Use regex to pull the name (the third word in the line)
  # Example: "## COLUMN2.name MAK" -> "MAK"
  col_names <- gsub("^## COLUMN\\d+\\.name\\s+(\\S+).*", "\\1", name_lines)
  
  # 3. Identify where the data starts
  # Find the first line that does NOT start with #
  data_start_idx <- grep("^[^#]", all_lines)[1]
  
  # 4. Read the data portion
  # We read from the identified start index to the end
  # Atlantis files are usually space-separated (multiple spaces allowed)
  data_text <- all_lines[data_start_idx:length(all_lines)]
  
  # Convert the text lines into a data frame
  df <- read.table(text = data_text, header = FALSE, sep = "")
  
  # 5. Assign the extracted column names
  if (length(col_names) == ncol(df)) {
    colnames(df) <- col_names
  } else {
    warning("Number of extracted names does not match number of data columns.")
  }
  
  return(df)
}

# Example Usage:
catch_data <- load_atlantis_catch(here::here('currentVersion','CatchFiles',"total_catch.ts"))

catch_tot = catch_data |> 
  tidyr::pivot_longer(-Time) |> 
  mutate(date = as.POSIXct(Time*86400, origin = '1964-01-01', tz = 'UTC'),
         year = format(date,format = '%Y')) |> 
  group_by(year) |> 
  summarise(catch.mgS = sum(value,na.rm=T),
            catch.mTd = catch.mgS * 0.0000864 * 5.7 * 20)

#Get box area
bgm = rbgm::bgmfile(here::here('currentVersion','neus_tmerc_RM2.bgm'))

box.km2 = bgm$boxes |>  filter (.bx0 %in% 1:22) |> 
  pull(area) |> 
  sum()*1E-6

#get catch per area

catch_area = catch_tot |> 
  mutate(catch.km2 = catch.mTd/box.km2) |> 
  filter(year >= 1985)

ggplot(catch_area, aes(x = as.numeric(year), y = catch.km2))+
  geom_line()

summary(catch_area$catch.km2)
sd(catch_area$catch.km2)
