#################################################################
# Function to print tables from a cox.ph object.
# aAuthor : koen.vanbrabant@kuleuven.be
# Date : 12-04-2017
###############################################################


coxph_table = function(data=data,fit=fit,roundings=3){
    
    require(Hmisc)
    require(survival)
    
    model_variables = attr(fit$terms,"term.labels")
    
    summary_table = summary(fit)$conf.int
    
    table = vector('list',length(model_variables))
    

    for (i in 1:length(model_variables)){
        if (is.factor(data[,model_variables[i]])){
            
            
            table[[i]] = as_tibble(matrix(NA,
                nlevels(data[,model_variables[i]])+1,5))
            
            names(table[[i]]) = c('Variable','value','Hazard Ratio','95% CI',
                'P-value')
            
            table[[i]][1,1] = ifelse(label(data[,model_variables[i]])=='',
                model_variables[i],label(data[,model_variables[i]]))
            
            # @ overal p-value
            test = Anova(mf_timevar_ir.fit2,test.statistic = 'Wald')
            
            logical_row = grepl(model_variables[i],rownames(test))

    
            table[[i]][1,5] = format_pval.table(test[logical_row,3])
            
        
            table[[i]][1,2:4] = ''
            
            table[[i]][2:nrow(table[[i]]),1] = ''
            
            table[[i]][2:nrow(table[[i]]),2] = levels(data[,model_variables[i]]) 
            
            table[[i]][2,3:5] = '#'
            
            spfc_smmry = grepl(model_variables[i],rownames(summary_table))
            
            table[[i]][3:nrow(table[[i]]),3] = round(
                summary_table[spfc_smmry,1],roundings)
            
            lower_ci = round(summary_table[spfc_smmry,3],roundings)
            upper_ci = round(summary_table[spfc_smmry,4],roundings)
            
            table[[i]][3:nrow(table[[i]]),4] = paste0(lower_ci,';',upper_ci)
            
            
            table[[i]][3:nrow(table[[i]]),5] = sapply(1:sum(spfc_smmry),
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
            
            table[[i]][1,3] = round(summary_table[spfc_smmry,1],roundings)
            
            lower_ci = round(summary_table[spfc_smmry,3],roundings)
            upper_ci = round(summary_table[spfc_smmry,4],roundings)
            
            table[[i]][1,4] = paste0(lower_ci,';',upper_ci)


            table[[i]][1,5] = format_pval.table(
                summary(fit)$coefficients[spfc_smmry,5])
            
        }
    }
    return(do.call('rbind',table))
}




