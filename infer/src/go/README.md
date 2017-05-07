## Beginning of a Go frontend to Infer

Our starting point is an OCaml parser and AST for a subset of Go, which was developed as part of the
McGill COMP520 course requirements by Rohan Jacob-Rao, Steven Thephsourinthone and Shawn Otis.
This included a type checker, weeders and generation of C code, though we will only use the frontend
to hook up with Infer.

You can test the current system using the following steps.

Step 1: Compile the project using `make` from this directory.  
This will create an executable called 'main.native'.

Step 2: Translate a Go program in the supported syntax set.  
Examples can be found in infer/infer/tests/codetoanalyze/go.  
For example, run `./main.native ../../tests/codetoanalyze/go/triangles.go`.

Step 3: Compile and run the generated C code.  
Generated files are stored at the location of the input program.  
Change to that directory: `cd ../../tests/codetoanalyze/go/`.  
Compile the C output file: `gcc triangles.c`.  
Run it: `./a.out`.

Experiment with parameters in the main function of the Go source file and repeat Steps 2 and 3.  
(Note that imported packages are currently not supported by our compiler. Hence the example programs
do not `import "fmt"` and instead translate `print` and `println` functions as if they were Go primitives.)
