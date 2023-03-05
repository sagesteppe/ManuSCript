library(DiagrammeR)

setwd('/home/sagesteppe/Documents/ManuSCript/script')

grViz("

digraph boxes_and_circle {
  
  node [shape = rectangle, fontsize = 36]
  
      # fieldwork operations
      subgraph cluster_fieldwork {
        style=dashed; color= '#ffa600'; 
        node [fontname = arial, style = filled, fontcolor = White, color = '#ffa600']
        
          'field work' -> 'plant survey' [penwidth = 6]
          'plant survey' -> 'herbarium samples' [label = '  vouchers',
              fontsize = 36, penwidth = 6]
          'field work' -> 'pollinator obs.' [penwidth = 6]
          'pollinator obs.' -> 'pollen samples' [label = '  capture insects', 
             fontsize = 36, penwidth = 6]
      }
        
      # Main spatial operations
      subgraph cluster_spatial {
      style=dashed; color= '#ff764a'; 
      node [fontname = arial, style = filled, fontcolor = White, color = '#ff764a']
      
      'distance query' -> 'Species Distribution Models' [label = '  log. regression', 
          fontsize = 36, penwidth = 6]
      }
      
      subgraph cluster3 {
      style=dashed; color= '#ef5675';
      node [fontname = arial,  style = filled, fontcolor = White, color = '#ef5675']

        'herbarium samples' -> 'DNA extraction' [penwidth = 6]
        'DNA extraction' -> 'hyb-seq'  [label = '  CTAB',  fontsize = 36, penwidth = 6]
        'hyb-seq' -> 'mi-Seq' [label = '  A353', fontsize = 36, penwidth = 6]
      }
      
      subgraph cluster_morphology {
      style=dashed; color= '#bc5090';
      node [fontname = arial, style = filled, fontcolor = White, color = '#bc5090']
      
        'herbarium samples' -> 'pollen morpho' [penwidth = 6]
        'pollen morpho' -> 'dichotomous key' [label = '  cluster', 
            fontsize = 36, penwidth = 6]
        'dichotomous key' -> 'count grains' [label = '  microscopy', 
            fontsize = 36, penwidth = 6]
        'pollen samples' -> 'count grains' [penwidth = 6]
      }
      
      subgraph cluster_phenology {
      style=dashed; color= '#7a5195'; pendwidth = 6;
      node [fontname = arial, style = filled, fontcolor = White, color = '#7a5195']
      
        'Species Distribution Models' -> 'museum records' [penwidth = 6]
        'museum records' -> 'weibull distributions' [label = '  subset geographically', 
            fontsize = 36, penwidth = 6]
      }

      subgraph cluster_bioinformatics {
      style=dashed; color= '#374c80';
      node [fontname = arial, style = filled, fontcolor = White, color = '#374c80']
       
        'mi-Seq' -> 'trimmomatic' [penwidth = 6]
        'trimmomatic' -> 'mega353' [fillcolor = red, penwidth = 6]
        'Species Distribution Models' -> 'NCBI SRA' [label = '  evaluation', fontsize = 36, penwidth = 6]
        'NCBI SRA' -> 'trimmomatic' [label = '  download', fontsize = 36, penwidth = 6]
        'mega353' -> 'hyb-piper' [label = '  target file', fontsize = 36, penwidth = 6]
        'hyb-piper' -> 'kraken-DB' [penwidth = 6]
        'hyb-piper' -> 'BLAST-DB' [penwidth = 6]
      }
      
      subgraph cluster_reclassification {
      style=dashed; color = '#003f5c'
      node [fontname = arial, style = filled, fontcolor = White, color = '#003f5c']
      
        'BLAST-DB' -> 'classifications' [color = 'Crimson', penwidth = 6]
        'classifications' -> 'post-class' [color = 'Crimson', penwidth = 6]
        'weibull distributions' -> 'post-class' [label = '  overlap', 
        fontsize = 36, penwidth = 6]
        'Species Distribution Models' -> 'post-class' [label = '  surrogates',  
        fontsize = 36, penwidth = 6]
        'post-class' -> 'abundance' [color = 'Crimson', penwidth = 6]
        'count grains' -> 'abundance' [label = '  morphotypes', fontsize = 36, penwidth = 6]
        'pollinator obs.' -> 'abundance' [penwidth = 6]
        
      }

      # highlight pollen classification
      'pollen samples' -> 'DNA extraction' [color = 'Crimson', penwidth = 6]
      'trimmomatic' -> 'kraken-DB'  [color = 'Crimson', penwidth = 6]
      'kraken-DB' -> 'BLAST-DB'  [color = 'Crimson', penwidth = 6]
}
") %>%  DiagrammeRsvg::export_svg() %>%
  charToRaw %>% 
  rsvg::rsvg_png("../graphics/plots/flowchart.png", width = 1920, height = 1080)

rm(g)