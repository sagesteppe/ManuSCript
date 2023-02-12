library(DiagrammeR)

setwd('/home/sagesteppe/Documents/ManuSCript/script')

g <- grViz("

digraph boxes_and_circle {
  
  node [shape = rectangle, fontsize = 18]
  
      # fieldwork operations
      subgraph cluster_fieldwork {
        style=dashed; color= '#ffa600'; pendwidth = 3
        node [fontname = arial, style = filled, fontcolor = White, color = '#ffa600']
        
          'field work' -> 'plant survey'
          'plant survey' -> 'herbarium samples' [label = 'vouchers', fontsize = 18]
          'field work' -> 'pollinator obs.'
          'pollinator obs.' -> 'pollen samples' [label = 'capture insects', fontsize = 18]
      }
        
      # Main spatial operations
      subgraph cluster_spatial {
      style=dashed; color= '#ff764a'; pendwidth = 3
      node [fontname = arial, style = filled, fontcolor = White, color = '#ff764a']
      
      'distance query' -> 'SDM' [label = 'log. regression',  fontsize = 18]
      }
      
      subgraph cluster3 {
      style=dashed; color= '#ef5675'; pendwidth = 3
      node [fontname = arial,  style = filled, fontcolor = White, color = '#ef5675']

        'herbarium samples' -> 'DNA extraction' 
        'DNA extraction' -> 'hyb-seq'  [label = 'CTAB',  fontsize = 18]
        'hyb-seq' -> 'mi-Seq' [label = 'A353', fontsize = 18]
      }
      
      subgraph cluster_morphology {
      style=dashed; color= '#bc5090'; pendwidth = 3
      node [fontname = arial, style = filled, fontcolor = White, color = '#bc5090']
      
        'herbarium samples' -> 'pollen morpho'
        'pollen morpho' -> 'dichotomous key' [label = 'cluster', fontsize = 18]
        'dichotomous key' -> 'count grains' [label = 'microscopy', fontsize = 18]
        'pollen samples' -> 'count grains'
      }
      
      subgraph cluster_phenology {
      style=dashed; color= '#7a5195'; pendwidth = 3
      node [fontname = arial, style = filled, fontcolor = White, color = '#7a5195']
      
        'SDM' -> 'museum records' 
        'museum records' -> 'weibull distributions' [label = 'subset geographically', fontie = 18]
      }

      subgraph cluster_bioinformatics {
      style=dashed; color= '#374c80'
      node [fontname = arial, style = filled, fontcolor = White, color = '#374c80']
       
        'mi-Seq' -> 'trimmomatic'
        'trimmomatic' -> 'mega353' [fillcolor = red]
        'SDM' -> 'NCBI SRA' [label = 'evaluation', fontsize = 18]
        'NCBI SRA' -> 'trimmomatic' [label = 'download', fontsize = 18]
        'mega353' -> 'hyb-piper' [label = 'target file', fontsize = 18]
        'hyb-piper' -> 'kraken-DB'
        'hyb-piper' -> 'BLAST-DB'
      }
      
      subgraph cluster_reclassification {
      style=dashed; color = '#003f5c'
      node [fontname = arial, style = filled, fontcolor = White, color = '#003f5c']
      
        'BLAST-DB' -> 'classifications' [color = 'Crimson', penwidth = 2]
        'classifications' -> 'post-class' [color = 'Crimson', penwidth = 2]
        'weibull distributions' -> 'post-class' [label = 'overlap',  fontsize = 18]
        'SDM' -> 'post-class' [label = 'surrogates',  fontsize = 18]
        'post-class' -> 'abundance' [color = 'Crimson', penwidth = 2]
        'count grains' -> 'abundance' [label = 'morphotypes', fontsize = 18]
        'pollinator obs.' -> 'abundance'
        
      }

      # highlight pollen classification
      'pollen samples' -> 'DNA extraction' [color = 'Crimson', penwidth = 2]
      'trimmomatic' -> 'kraken-DB'  [color = 'Crimson', penwidth = 2]
      'kraken-DB' -> 'BLAST-DB'  [color = 'Crimson', penwidth = 2]
}
")

g %>%
  DiagrammeRsvg::export_svg() %>%
  charToRaw %>% 
  rsvg::rsvg_png("../graphics/plots/flowchart.png", width = 420, height = 1260)

