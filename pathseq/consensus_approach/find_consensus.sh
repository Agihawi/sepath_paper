#!/bin/bash

#obtain consensus
for x in '001' '002' '003' '004' '011' '012' '014' '015' '022' '024' '025'; do Rscript comp_consensus.R motu_genus/$x\.comp pathseq_1filt/$x\_filt.comp spades_kraken_4_thresh/$x\_4thresh.comp consensus_comp/$x\_consensus.comp; done

#measure performance
for x in '001' '002' '003' '004' '011' '012' '014' '015' '022' '024' '025'; do Rscript comp_performance2.R consensus_comp/$x\_consensus.comp true_comp/$x\.comp > consensus_comp/$x\_report; done

$ mv performancelog consensus_comp/
