# 2025-10-21 6:34

I know the project will use prefix notation as its main form of expression and use. I am still in the planning phase of this project as I understand the rquirements of this project. Just wanted to get my directory started to get this rolling and my devlog created.

 ### Fri Oct 24 19:08:59 CDT 2025 - Testing for 500th time
 ### Fri Oct 24 19:09:29 CDT 2025 - test me
 ### Fri Oct 24 19:09:54 CDT 2025 - Finally
 ### Fri Oct 24 19:10:18 CDT 2025 - message
 ### Fri Oct 24 19:10:41 CDT 2025 - OK
 ### Fri Oct 24 19:11:00 CDT 2025 - PLease
 ### Fri Oct 24 19:21:49 CDT 2025 - This should work
 ### Fri Oct 24 19:34:40 CDT 2025 - YES IT WORKS
Figured out alias was working the entire time was just opening and commiting in the wrong directory. Because of this also had to resolve git issues that are finally resolved I believed. Good news is that I have a plan and most of the code done before messing with all this git nonsense.

 ### Fri Oct 24 19:46:55 CDT 2025 - Created file structure
 Created my basic file structure for the project. Will be adding more in the future just wanted to lay the building blocks for the future.



 Goal for this session: Implement startup logic for detecting interactive vs batch mode.

 Used getArgs to capture command line flags 
 Determined mode using elem on -b and --batch
 Implemented evalLoop that currently just echoes input
 Added quit condition and prompt handling for interactive mode

 Created the foundation for the calcs REPL behavior. The evalLoop recursion feels simple and right in Haskell. Next step will be to integrate a history list for the calc that records results and dislays them with increment

