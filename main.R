library("viridis")
library("Biostrings")
library("DECIPHER")
library("seqinr")
library("ade4")
library("adegenet")
library("ape")
library("ggtree")
library("ggplot2")

rm(list =)

viruses <- c("NC_045512", "OQ082914.1", "OQ826138.1", "BS007314", "OX490409.1", "OL989090.1", "OQ859931.1", "OM146071.1", "MK967708.1")
virus_sequences <- read.GenBank(viruses)

write.dna(virus_sequences, file = "viruses.fasta", format = "fasta") 
virus_seq_not_align <- readDNAStringSet("viruses.fasta", format = "fasta")
vector_lect <- read.fasta("viruses.fasta") 
virus_vector <- vector_lect[[1]]

length_data_vector <- c() 
gc_content_vector <- c() 
count_vector <- c() 
a <- c()
c <- c()
g <- c()
t <- c()

print("Escritos en orden A, C, G y T")
for (i in 1:length(viruses)) {
   length_vector <- length(virus_sequences[[i]])
   
   
   gc_content_vector <- append(gc_content_vector, values = GC(vector_lect[[i]]))
   count_vector <- append(count_vector, count(virus_lect[[i]], 1))
   length_data_vector <- append(length_data_vector, length_vector)
   
   
   cat(viruses[[i]], ":", GC(virus_lect[[i]], 1), "\n")
   
   
}

for (i in 1:length(viruses)) {
  cat(viruses[[i]], ":", count(virus_lect[[i]], 1), "\n")
}


x = 1 

count_vector
for (i in 1:length(count_vector)) { 

  
  # print(count_vector[[i]])
  if (x == 1) { a <- append(a, count_vector[[i]]) }
  if (x == 2) { c <- append(c, count_vector[[i]]) }
  if (x == 3) { g <- append(g, count_vector[[i]]) }

  if (x >= 4) { 
    t <- append(t, count_vector[[i]])     
    x = 0
    
    }
  x = x + 1 
}
virus_nombres_comunes <- c("SARS-COV2 (Wuhan, China)", "SARS-COV2 (Mexico)", "SARS-COV2 (South Africa)", "SARS-COV2 (Japan)", "SARS-COV2 (United Kingdom)", "SARS-COV2 (The Philippines)", "SARS-COV2 (India)", "SARS-COV2 (Chile)", "MERS-Cov (Middle East)")
virus_nombres_comunes 

df <- data.frame(NombreComun = virus_nombres_comunes, Virus = viruses, Longitud = length_data_vector, GC = gc_content_vector, A = a, G = g, C = c, T = t)

plot_table <- ggplot(df, aes(x = Virus, y = Longitud, fill = Virus)) + geom_bar(stat = "identity")
plot_table

data_table_a <- data.frame(Nombre = virus_nombres_comunes, A = a)

data_table_g <- data.frame(Nombre = virus_nombres_comunes, G = g)

data_table_c <- data.frame(Nombre = virus_nombres_comunes, C = c)

data_table_t <- data.frame(Nombre = virus_nombres_comunes, T = t)


plot_a <- ggplot(data_table_a, aes(x = viruses, y = a, fill = viruses)) + geom_bar(stat = "identity") 
plot_a

plot_g <- ggplot(data_table_elements, aes(x = viruses, y = g, fill = viruses)) + geom_bar(stat = "identity") 
plot_g

plot_c <- ggplot(data_table_elements, aes(x = viruses, y = c, fill = viruses)) + geom_bar(stat = "identity") 
plot_c

plot_t <- ggplot(data_table_elements, aes(x = viruses, y = t, fill = viruses)) + geom_bar(stat = "identity") 
plot_t

