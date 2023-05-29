library("viridis")
library("Biostrings")
library("DECIPHER")
library("seqinr")
library("ade4")
library("adegenet")
library("ape")
library("ggtree")
library("ggplot2")

viruses <- c("NC_045512", "OQ082914.1", "OQ826138.1", "BS007314", "OX490409.1", "OL989090.1", "OQ859931.1", "OM146071.1", "MK967708.1")
virus_sequences <- read.GenBank(viruses)

write.dna(virus_sequences, file = "listaVirus.fasta", format = "fasta")

virus_seq_not_align <- readDNAStringSet("listaVirus.fasta", format = "fasta")
class(virus_seq_not_align)
virus_seq_not_align

virus_seq_not_align <- OrientNucleotides(virus_seq_not_align)

virus_seq_align <- AlignSeqs(virus_seq_not_align)
BrowseSeqs(virus_seq_align)

writeXStringSet(virus_seq_align, file = "virus_sequence_aligned.fasta")  
virus_aligned <- read.alignment("virus_sequence_aligned.fasta", format = "fasta")

# Punto 4 
matriz_distancia <- dist.alignment(virus_aligned, matrix = "similarity")
matriz_distancia 

temp <- as.data.frame(as.matrix(matriz_distancia))
temp

table.paint(temp, cleg = 0, clabel.row = .5, clabel.col = .5) + scale_color_viridis()

virus_tree <- nj(matriz_distancia)
class(virus_tree) # phylo

ggtree(virus_tree, branch.length = "none", layout = "circular") + geom_tiplab()
plot(virus_tree)
