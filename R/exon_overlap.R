exon_overlap <- function(gene_A_exons, gene_B_exons){

  if(dim(gene_A_exons)[1] == 0 | dim(gene_B_exons)[1] == 0){
    return (FALSE)
  }

  for(row_exonA in 1:nrow(gene_A_exons)){
    for(row_exonB in 1:nrow(gene_B_exons)){

      # Safe sequence generation for gene A exon
      if (gene_A_exons[row_exonA,1] <= gene_A_exons[row_exonA,2]-1) {
        x = seq(from = gene_A_exons[row_exonA,1], to = gene_A_exons[row_exonA,2]-1, by = 1)
      } else {
        x = integer(0)  # Empty sequence for invalid coordinates
      }
      
      # Safe sequence generation for gene B exon
      if (gene_B_exons[row_exonB,1] <= gene_B_exons[row_exonB,2]-1) {
        y = seq(from = gene_B_exons[row_exonB,1], to = gene_B_exons[row_exonB,2]-1, by = 1)
      } else {
        y = integer(0)  # Empty sequence for invalid coordinates
      }

      if(length(intersect(x,y))!=0){
        return (TRUE)
      }
    }
  }
  return (FALSE)
}
