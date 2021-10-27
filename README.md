# DNA-Header
<img src="https://github.com/SRingshandl/DNA-Header/blob/main/generated_output.gif" width="100%">

Small project for an animated gif above my github page

The program creates a specified number of png pictures in an output folder via R and merges them into a gif file via python.  
The python script is called from the R program.  

Pictures contain letters A, T, G and C in moving circles that connect to others via lines when in close proximity.  
Transparency of lines is corresponding to distance between circles.  
Upon leaving the specified area a new letter is created with randomized speed vectors. 
