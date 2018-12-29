Thomas Burt
CSC 254
December 2, 2018
README

Test Cases:
1. ab.c
a. Passes
2. loop_while.c
a. passes
3. MeaningOfLife.c
a. passes

Note: At any point I state "set a value" in this, it means I am outputting a string of a value being set.  e.g. "base = top;" would be outputted when I say I set base equal to top.

Transformation:
For this project, there was much to change in switching the program from a translator to a compiler.  
The first thing that was needed was a change variable output for the compiler.  I got rid of the local and global array 
instantiations, as we were only allowed to use Mem for the one function.  I then had Mem instantiated before the main function 
is.  I keep track of the size of the mem array by keeping a reference variable of the count number 
for the number of variables.  I then sneak this value into being printed with a nice match to the string 
“int mem[“ in my print_parse_list function.  This function is where the printing of the translation is done.  
The program function simply adds each line of the translation into a reference array of strings known as output.  Also, since I needed to keep
track of local and parameter variable counts, the statements were kept in a state_list to be printed out after finding the count, so that the count may be used
for the caller re-entry part that I made later on.  The new program also needed variables base and top, which were to be used for stacks changes.
These were key for allowing memory to be allocated for parameters, return values and return addresses when a function was called inside another. 
Once variable names were changed, parameter and local variable counts were checked and stored, you would then need to focus your attention on the function calls.
This meant making sure function code memory allocation code was made at the beginning of the main function.  This meant allocating three spots in the mem array for return 
values of any callee function and the return addresses for any callee function. Main function is the first function also called.  Base and top would be set to 3 as
global parameters, just before "int main{" was outputted. I did it this way however, I couldve done #define base 3 at the start too, or just instantiated them right after
main is outputted.  Then "goto mainfunc;" is outputted before any functions are done so that the program will start at mainfunc, the main function of any program.
  Follow this, I had to setup setup function that were not main.  For this, I made sure there a function to do the Callee prologue part and the returning part of a function,
if there was one.  This is where the saving of statements into "state_list" comes into play.  At the start of any function I had base set to top, then top set to its 
local variable count plus itself.  Because this needed to be done at the beginning of the function with the use of local variable count, I made the state_list, statement 
list variable, which would be called to be outputted after the previous base and top had been set.  Following this was the instantiation of parameters of variables.  
How I did this was I had a function, mem_Name, which would be called during data_decls to store in a hashtable mem[base+local_var_count] for all data variables.  The local
variable count reference variable was then increased by 1 each time the function was called. These stored values would later be called in expression, term and factor. 
For parameters, I would increment the reference variable for parameter_count and local count for each parameter found.  The reason for both is because to have consistency 
when using both parameters and local variables.  At the start of each function, base is set to equal top.  Top is set to equal the sum of itself and the local variable count.  
Then parameters were gathered from the parameter values stored by the caller function with the prejump code.  The callee function then translates it code accordingly, using the
symbtable as reference to variables.  Once the function was completed, the return value was set equal to mem[base-1], the designated return value allocation.  Then
base is set to top.  Finally, jumpreg, a defined variable right after the meta statements, is set to mem[base-2], the designated location for the caller return address, and then 
goto jumptabe is called.  After the first non-main function, the jumptable outputted, although it didn't really matter where it was located as long as 
it wasn't at the end of the program after main.  
  As for caller functions they had to do multiple things when a callee function is called.  First, base is set to top.  Then top is set to the sum of itself plus 3, for reutrn 
value and address locations.  Then, I called a function and with the help of my parameter count hashtable, which had a key of a function name, to set the parameters for the 
function being called, e.g. mem[base+0] = mem[top+0], etc. Then we push the parameters on the stack by settting top equal to the sum of itself and the callee parameter count.
Then mem[top] is set to base, mem[top+1] is set to the return address and top is increase by 3.  Then a goto function is outputted with the function name stored in my symbtable.
A label is outputted after this goto call, which the jumptable will then jump to.  Once jumped back to this label, my program then restores the base value, which is done by outputting
base = mem[top-3].  A return value is then copied into a newly created name and slot, in the MeaningOfLife instance, mem[base+2].  An output of an increase of top by 3 is then written
and the caller function then moves on to the next part of the compiling.  
   Dealing with multiple functions meant a complete change in output.  I got rid of the outputted functions 
themselves and formed their output into labels.  This was generated by concatenating “func” at the end of the 
functions name.  This would be the new label name for the function.  
When each function gets instantiated, memory allocation is necessary for the function.  We must allocate extra 
space for the parameters.  We arranged a “stack” to deal with this issue.  Local and parameter variable counts 
must be kept track of in order to successfully allocate this memory for the function.  This is then used to form 
values in the function.  The function then uses this allocated memory to execute a functions code.  Once completed 
it uploads the answer into the designated return value mem[base-1].  The address also has a designated slots, 
mem[base-2], which is used in the call to jumptable to distinguish the location, usually in mainfunc, of the function 
call.  The jumptable jumps to a label that is immediately stated after the function call inside the caller function.  This is 
how function calls are dealt with in a single function.  Labels and jumps with memory allocation.
	Finally, the translation builds the main function.  This is easily one at the start, once mem[] is instantiated, 
which is done right after all the meta statements were done.  Then, the main function is instantiated.  I myself, 
had 2 global variables which were instantiated after mem[] and just before main.  This then forms the shell of 
the rest of the code.  This is how the translation works.  
