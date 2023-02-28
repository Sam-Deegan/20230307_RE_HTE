
#TLDR: Purpose of the code is to set up the file structure for new GitHub Projects 

### Code Generating Root Folders ##############################################

# Define the top-level directories
directories <- c("0_Project_Management", "1_Build", "2_Analysis")

# Define the subdirectories for each top-level directory
subdirectories <- c("A_Input", "B_Code", "C_Output", "D_Temp")

# Create the directory structure
for (dir in directories) {
  # Create the top-level directory
  dir.create(dir)
  
  # Create the subdirectories within the top-level directory
  if (dir != "0_Project_Management") {
    for (subdir in subdirectories) {
      dir.create(file.path(dir, subdir))
    }
  }
}

cat(paste0("Project folders have been created."))
### Generate Project Readme Template ##########################################

# Define the project information
project_title <- "Project Name"
project_description <- "A brief summary of what the project does and why it exists."
installation_instructions <- "Instructions on how to install the project and any dependencies that are needed."
usage_examples <- "Examples of how to use the project, including code snippets or screenshots if applicable."
contributing_guidelines <- "Guidelines for how others can contribute to the project, including how to report bugs or submit feature requests."
credits_list <- "A list of the people or organizations who have contributed to the project, as well as any other resources or libraries that were used."
license_info <- "Information on the project's license, including any restrictions on how the code can be used or modified."
contact_info <- "How to get in touch with the project's author or maintainers, including email addresses or social media profiles."

# Define the README content
readme_contents <- paste("#", project_title, 
                         "\n\n", project_description, 
                         "\n\n## Table of Contents\n\n- [Installation](#installation)\n- [Usage](#usage)\n- [Contributing](#contributing)\n- [Credits](#credits)\n- [License](#license)\n- [Contact Information](#contact-information)\n\n## Installation\n\n", 
                         installation_instructions, "\n\n## Usage\n\n", 
                         usage_examples, "\n\n## Contributing\n\n", 
                         contributing_guidelines, "\n\n## Credits\n\n", 
                         credits_list, "\n\n## License\n\n", license_info, 
                         "\n\n## Contact Information\n\n", contact_info)

# Write the README file
write(readme_contents, file = "README.md")

cat(paste0("File '", file_name, "' has been created."))

### Create Project Plan #######################################################

# Write the README file
project_title <- "Project Name"
project_overview <- "A brief summary of the project, its goals, and its intended outcomes."
scope_and_deliverables <- "A detailed description of what the project will include and what the project team will deliver."
timeline <- "A timeline of the project, including key milestones and deadlines."
resources_and_budget <- "A list of the resources required for the project, including personnel, equipment, and materials, as well as the project budget."
risk_management <- "An analysis of the risks associated with the project, including potential challenges, and a plan to mitigate those risks."
communication_plan <-  "A plan for how the project team will communicate with each other, stakeholders, and other parties involved in the project."
project_organization <-  "An overview of the project team, including roles and responsibilities."
evaluation_and_metrics <- "A plan for evaluating the success of the project, including specific metrics for measuring progress and success."
next_steps <- "A plan for what will happen once the project is completed, including follow-up activities, maintenance, and ongoing support."


# Define the README content
project_plan_contents <- paste("#", project_title, 
                               "\n\n", project_overview, 
                               "\n\n## Table of Contents\n\n- [Scope and Deliverables](#scope-and-deliverables)\n- [Timeline](#timeline)\n- [Resources and Budget](#resources-and-budget)\n- [Risk Management](#risk-management)\n- [Communication Plan](#communication-plan)\n- [Project Organization](#project-organization)\n- [Evaluation and Metrics](#evaluation-metrics)\n- [Next Steps](#next-steps)\n",
                               "\n## Scope and Deliverables\n\n", scope_and_deliverables, 
                               "\n\n## Timeline\n\n", timeline, 
                               "\n\n## Resources and Budget\n\n", resources_and_budget, 
                               "\n\n## Risk Management\n\n", risk_management, 
                               "\n\n## Communication Plan\n\n", communication_plan, 
                               "\n\n## Project Organization\n\n", project_organization,
                               "\n\n## Evaluation and Metrics\n\n", evaluation_and_metrics, 
                               "\n\n## Next Steps\n\n", next_steps)

write(project_plan_contents , file = "0_Project_Management/Project_Plan.md")

cat(paste0("File '", file_name, "' has been created."))









