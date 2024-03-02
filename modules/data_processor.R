# data_processor_server = function(a, lrt_a, id = "data_processor"){
#   moduleServer(id, function(input, output, session){
#     observe({
#       req(filtered_data(), .dt_count)
#       cat("Updating filtered features...\n")
#       .r$dds = filtered_data()
#       .re$genes <<- unique(.dt_count[feature_id %in% rownames(filtered_data()), gene_id])
#     })
#     
#     observe({
#       req(.r$dds, lrt_a())
#       cat("Updating dt_lrt_sig...\n")
#       .re$dt_lrt_sig <<- get_dt_lrt(.r$dds, lrt_a())
#     })
#     
#     observe({
#       req(.r$dds, .re$dt_lrt_sig)
#       cat("Updating dds_lrt_sig...\n")
#       .r$dds_lrt_sig = .r$dds[.re$dt_lrt_sig[, feature_id], ]
#     })
#     
#     observe({
#       req(.r$dds_lrt_sig)
#       cat("Updating res_lrt...\n")
#       .r$res_lrt = get_res(.r$dds_lrt_sig)
#     })
#     
#     observe({
#       req(.r$dds)
#       cat("Updating res_all...\n")
#       .r$res_all = get_res(.r$dds)
#     })
#     
#     observe({
#       req(.r$res_lrt)
#       cat("Updating dt_res_lrt...\n")
#       .re$dt_res_lrt <<- dt_all_results(.r$res_lrt)
#     })
#     
#     observe({
#       req(.r$res_all)
#       cat("Updating dt_res_all...\n")
#       .re$dt_res_all <<- dt_all_results(.r$res_all)
#     })
#     
#     observe({
#       # if(only_sig()){
#       #   req(.re$dt_res_lrt)
#       #   cat("Updating dt_res with dt_res_lrt...\n")
#       #   .r$dt_res = .re$dt_res_lrt 
#       # }
#       # else{
#       req(.re$dt_res_all)
#       cat("Updating dt_res with dt_res_all...\n")
#       .r$dt_res = .re$dt_res_all 
#       # }
#     })
#     
#     
#   })
# }