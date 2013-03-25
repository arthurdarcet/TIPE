ocamlc.opt -w p -I +labltk -o cxa-ppm.exe labltk.cma unix.cma str.cma types.ml operations_de_base.ml divers.ml huffman.ml destructive.ml acp.ml cxa_image.ml ppm_image.ml interface.ml

ocamlc.opt -w p -o compression.exe unix.cma str.cma types.ml operations_de_base.ml divers.ml huffman.ml destructive.ml acp.ml cxa_image.ml ppm_image.ml compression.ml

ocamlc.opt -w p -o decompression.exe unix.cma str.cma types.ml operations_de_base.ml divers.ml huffman.ml destructive.ml acp.ml cxa_image.ml ppm_image.ml decompression.ml

del *.cmx
del *.cmi
del *.o
del *.cmo