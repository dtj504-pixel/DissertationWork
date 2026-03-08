# Importing helpful libraries
# glob helps hunt for file types
import glob
# os lets python access my oeprating system to get file names
import os

# Defining folder path
log_folder_path = "/home/emma/Downloads/Downloads_1/Third_Year_Maths/Dissertation/Two_stocks_Optimising_ftarget_in_shortcut_model_first_1000_runs/*.log" 
output_folder = "/home/emma/Downloads/Downloads_1/Third_Year_Maths/Dissertation/Analysing_results"
output_file = os.path.join(output_folder, "log_analyser_Two_stocks_Optimising_ftarget_in_shortcut_model.txt")


# create an empty dictionary to put the results in
results = {}

# Check how many files we have to process
print(f"I found {len(glob.glob(log_folder_path))} files to check!")

# Loop through every .log file in the folder
for filepath in glob.glob(log_folder_path):
    # Record the filename for later use in the dictionary of results
    filename = os.path.basename(filepath)
    # open the file and automatically close it afterwards
    with open(filepath, 'r') as file:
        # don't capture data until we find the part fo the file we want
        capturing_data = False
        # start list for captured lines to go into
        captured_lines = []
        # read the file line by line
        for line in file:
            # Check if we should start recording
            if "Optimal parameters:" in line:
                # start recording
                capturing_data = True
                # Skips the header line itself so it isn't in our data
                continue 
            
            # If our switch is on, start handling the lines
            if capturing_data:
                # If the line is empty, the table is done so we stop recording
                if line.strip() == "":
                    break 
                
                # Add the lines form the table to our list of captured lines
                captured_lines.append(line.strip())
                # See what's being captured
                print(f"Captured lines from {filename}")  
                print(captured_lines)
        
        # Join the list of lines into a table (the newline character) and save it
        if captured_lines:
            results[filename] = "\n".join(captured_lines)

# Put all results into a set to compare them
# Then, if the size of the set is 1, they all have the same optimal parameters
unique_parameters = set(results.values())

# print the results of the analysis
print("--- ANALYSIS COMPLETE ---")

# Open our new text file in 'w' (write) mode
with open(output_file, 'w') as out_file:

    # print a message if there are no parameteres found
    if len(unique_parameters) == 0:
        print("No parameters were found in any of the files.")
        out_file.write("No parameters were found in any of the files.\n")
    # print a message for the successful case
    elif len(unique_parameters) == 1:
        print("Success! All log files have the exact same optimal parameters:\n")
        print(list(unique_parameters)[0])
        out_file.write("Success! All log files have the exact same optimal parameters:\n")
        out_file.write(list(unique_parameters)[0] + "\n") 
    # print a message if more than one unique set of parameters is found, and print the parameters for each file
    else:
        print("Differences found! Here is the breakdown:\n")
        out_file.write("Differences found! Here is the breakdown:\n")
        for file, param in results.items():
            print(f"--- {file} ---")
            out_file.write(f"--- {file} ---\n")
            print(f"{param}\n")
            out_file.write(f"{param}\n\n")

print(f"\n Results have been saved to: {output_file}")