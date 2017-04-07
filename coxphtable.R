#################################################################
# Function to print tables from a cox.ph object.
# aAuthor : koen.vanbrabant@kuleuven.be
# Date : 07-04-2017
###############################################################

coxph_table = function(data=data,fit=fit){
    
    require(Hmisc)
    
    model_variables = attr(fit$terms,"term.labels")
    
    summary_table = summary(fit)$conf.int
    
    table = vector('list',length(model_variables))
    

    for (i in 1:length(model_variables)){
        if (is.factor(data[,model_variables[i]])){
            
            table[[i]] = as_tibble(matrix(NA,
                nlevels(data[,model_variables[i]]),5))
            
            names(table[[i]]) = c('Variable','value','Hazard Ratio','95% CI',
                'P-value')
            
            table[[i]][1,1] = ifelse(label(data[,model_variables[i]])=='',
                model_variables[i],label(data[,model_variables[i]]))
            
            table[[i]][,2] = levels(data[,model_variables[i]]) 
            
            table[[i]][1,3:5] = '#'
            
            spfc_smmry = grepl(model_variables[i],rownames(summary_table))
            
            table[[i]][2:nrow(table[[i]]),3] = round(
                summary_table[spfc_smmry,1],2)
            
            lower_ci = round(summary_table[spfc_smmry,3],2)
            upper_ci = round(summary_table[spfc_smmry,4],2)
            
            table[[i]][2:nrow(table[[i]]),4] = paste0(lower_ci,';',upper_ci)
            
            
            table[[i]][2:nrow(table[[i]]),5] = sapply(1:sum(spfc_smmry),
                function(x){
                format_pval.table(summary(fit)$coefficients[spfc_smmry,5][x])
            
            })
            
        
        }else{
            table[[i]] = as_tibble(matrix(NA,1,5))
            
            names(table[[i]]) = c('Variable','value','Hazard Ratio','95% CI',
                'P-value')
            
            table[[i]][1,1] = ifelse(label(data[,model_variables[i]])=='',
                model_variables[i],label(data[,model_variables[i]]))
            
            table[[i]][1,2] = ''
            
            spfc_smmry = grepl(model_variables[i],rownames(summary_table))
            
            table[[i]][1,3] = round(summary_table[spfc_smmry,1],2)
            
            lower_ci = round(summary_table[spfc_smmry,3],2)
            upper_ci = round(summary_table[spfc_smmry,4],2)
            
            table[[i]][1,4] = paste0(lower_ci,';',upper_ci)


            table[[i]][1,5] = format_pval.table(
                summary(fit)$coefficients[spfc_smmry,5])
            
        }
    }
    return(do.call('rbind',table))
}




