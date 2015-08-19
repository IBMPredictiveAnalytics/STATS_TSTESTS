#Licensed Materials - Property of IBM
#IBM SPSS Products: Statistics General
#(c) Copyright IBM Corp. 2015
#US Government Users Restricted Rights - Use, duplication or disclosure 
#restricted by GSA ADP Schedule Contract with IBM Corp.

# Author: JKP, IBM SPSS
# Version = 1.0.0

# history
# 02-feb-2015 - original version



tstests <- function(variable, 
            cointvars=NULL, pointercept=TRUE, polag="short",
            adfalth=NULL, adflagorder=NULL,
            ppalth=NULL, pptype="zalpha", pplag="short",
            kpssnull=NULL, kpsslag="short", falsecointvars=NULL
                    ) {

    setuplocalization("STATS_TSTESTS")	

    # A warnings proc name is associated with the regular output
    # (and the same omsid), because warnings/errors may appear in
    # a separate procedure block following the regular output
    procname=gtxt("Time Series Tests")
    warningsprocname = gtxt("Time Series Tests: Warnings")
    omsid="STATSTSTESTS"
    warns = Warn(procname=warningsprocname,omsid=omsid)

    tryCatch(library(tseries), error=function(e){
        warns$warn(gtxtf("The R %s package is required but could not be loaded.","tseries"),
            dostop=TRUE)
    }
    )

    dopo = !is.null(cointvars)
    doadf = !is.null(adfalth)
    dopp = !is.null(ppalth)
    dokpss = !is.null(kpssnull)
    allargs = as.list(environment())
    
    if (!is.null(falsecointvars)) {
        warns$warn(gtxt("Cointegration variables were specified but the test was not selected"),
            dostop=TRUE)
    }
    if (!any(dopo, doadf, dopp, dokpss)) {
        warns$warn(gtxt("At least one test must be specified"), dostop=TRUE)
    }
    if (length(cointvars) > 5) {
        warns$warn(gtxt("At most 5 cointegration variables may be specified"),
            dostop=TRUE)
    }

    respo = resadf = respp = reskpss = NULL
    dta = spssdata.GetDataFromSPSS(c(variable, cointvars), missingValueToNA=TRUE)
    if (any(sapply(dta, is.na))) {
        warns$warn(gtxt("Missing values are not allowed in this procedure"), dostop=TRUE)
    }
    # painful code alert!
    # The functions called below sometimes return warnings as well
    # as the desired result.  The code captures the warning, if any, and
    # adds it to the result object for each function. 
    # Using tryCatch here would cause the result to be replaced by the warning.

    # Phillips-Ouliaris test
    if (dopo) {
        ww = NULL
        respo = withCallingHandlers(
            po.test(dta, demean=pointercept, lshort=polag=="short"),
            error=function(e) {warns$warn(e$message, dostop=TRUE)},
            warning = function(w) {ww<<-w}
        )
#         respo = tryCatch(po.test(dta, demean=pointercept, lshort=polag=="short"),
#             error=function(e) {warns$warn(e$message, dostop=TRUE)}
#         )
        respo$warnings = ww
    }

    # Phillips-Perron test
    if (dopp) {
        ww = NULL
        respp = withCallingHandlers(
            pp.test(
                dta[,1], 
                alternative=ppalth, 
                type=ifelse(pptype =="zalpha", "Z(alpha)", "Z(t_alpha)"),
                lshort = pplag=="short"),
            error=function(e) {warns$warn(e$message, dostop=TRUE)},
            warning = function(w) {ww<<-w}
            )
        respp$warnings = ww
    }
    
    # Augmented Dickey-Fuller test
    if (doadf) {
        if (is.null(adflagorder)) {
            adflagorder = trunc((nrow(dta)-1)^(1/3))
        }
        ww = NULL
        resadf = withCallingHandlers(
            adf.test(
            dta[,1], 
            alternative=adfalth, k=adflagorder),
            error=function(e) {warns$warn(e$message, dostop=TRUE)},
            warning = function(w) {ww<<-w}
        )
        resadf$warnings = ww
    }
    
    #  Kwiatkowski-Phillips-Schmidt-Shin test level or trend stationary hyp
    if (dokpss) {
        ww = NULL
        reskpss = withCallingHandlers(
            kpss.test(
            dta[,1],
            null=ifelse(kpssnull == "level", "Level", "Trend"),
            lshort=kpsslag == "short"),
            error=function(e) {warns$warn(e$message, dostop=TRUE)},
            warning = function(w) {ww<<-w}
                )
        reskpss$warnings = ww
    }
    
    # display results

    displayresults(allargs, respo, respp, resadf, reskpss, warns)

	###spsspkg.EndProcedure()
	warns$display(inproc=TRUE)

	
    res <- tryCatch(rm(list=ls()), warning = function(e) {return(NULL)})
}


displayresults = function(allargs, respo, respp, resadf, reskpss, warns) {
    # Display pivot table of results for all tests

    # allargs contains all the specifications
    # respo, respp, resadf, and reskpss contain the
    # output from each test function, including any warning
    # (It is assumed that only one warning can appear per test)
    # warns is the error message handler

    StartProcedure(gtxt("Time Series Tests"), "STATSTSTESTS")
    # summary results
    scaption = gtxt("Computations done by R package tseries")
    lbls = list()
    vals=list()
    if (!is.null(respo)) {
        lbls = c(
            gtxt("Test(1)"),
            gtxt("Variables(1)"),
            gtxt("P-Value(1)"),
            gtxt("Note(1))"),
            gtxt("Truncation Lag(1)"),
    		gtxt("Intercept Included(1)"))
        
        vals = c(
            gtxt("Phillips-Ouliaris Conintegration"),
            paste(allargs$cointvars, collapse="\n"),
            rounder(respo$p.value),
            ifelse(is.null(respo$warnings), gtxt("None"), respo$warnings[[1]]),
            respo$parameter,
            ifelse(allargs$pointercept, gtxt("Yes"), gtxt("No"))
        )
    }
    
    if(!is.null(respp)) {
        lblsx = c(
            gtxt("Test(2)"),
            gtxt("Alternative Hypothesis(2)"),
            gtxt("P-Value(2)"),
            gtxt("Note(2)"),
            gtxt("Truncation Lag(2)")
        )
        valsx = c(
            gtxt("Phillips-Perron"),
            ifelse(allargs$ppalth == "stationary", gtxt("Stationary"), gtxt("Explosive")),
            rounder(respp$p.value),
            ifelse(is.null(respp$warnings), gtxt("None"), respp$warnings[[1]]),
            respp$parameter
            
        )
        lbls = c(lbls, lblsx)
        vals = c(vals, valsx)
            
    }
    if (!is.null(resadf)) {
        lblsx = c(
            gtxt("Test(3)"),
            gtxt("Alternative Hypothesis(3)"),
            gtxt("P-Value(3)"),
            gtxt("Note(3)"),
            gtxt("Truncation Lag(3)")
            )
        valsx = c(
            gtxt("Augmented Dickey-Fuller"),
            ifelse(allargs$adfalth == "stationary", gtxt("Stationary"), gtxt("Explosive")),
            rounder(resadf$p.value),
            ifelse(is.null(resadf$warnings), gtxt("None"), resadf$warnings[[1]]),
            resadf$parameter
        )
        lbls = c(lbls, lblsx)
        vals = c(vals, valsx)
    }
    if (!is.null(reskpss)) {
        lblsx = c(
            gtxt("Test(4)"),
            gtxt("Null Hypothesis(4)"),
            gtxt("P-Value(4)"),
            gtxt("Note(4)"),
            gtxt("Truncation Lag(4)")
        )
        valsx = c(
            gtxt("KPSS Test for Stationarity"),
            ifelse(allargs$kpssnull == "level", gtxt("Level"), gtxt("Trend")),
            rounder(reskpss$p.value),
            ifelse(is.null(reskpss$warnings), gtxt("None"), reskpss$warnings[[1]]),
            reskpss$parameter
        )
        lbls = c(lbls, lblsx)
        vals = c(vals, valsx)
    }

    spsspivottable.Display(
        data.frame(cbind(vals), row.names=lbls), 
        title = gtxtf("Time Series Tests for Variable: %s", allargs$variable),
        collabels=c(gtxt("Values")), 
        templateName="TSTESTS", 
        outline=gtxt("Tests"),
        caption = scaption)

}


# localization initialization
setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
} 

# override for api to account for extra parameter in V19 and beyond
StartProcedure <- function(procname, omsid) {
    if (substr(spsspkg.GetSPSSVersion(),1, 2) >= 19) {
        spsspkg.StartProcedure(procname, omsid)
    }
    else {
        spsspkg.StartProcedure(omsid)
    }
}
gtxt <- function(...) {
    return(gettext(...,domain="STATS_TSTESTS"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="STATS_TSTESTS"))
}

rounder <- function(s) {
    if (!is.null(s)) {
        s = round(s, 5)
    } else {
        s = "."
    }
    return(s)
}
Warn = function(procname, omsid) {
    # constructor (sort of) for message management
    lcl = list(
        procname=procname,
        omsid=omsid,
        msglist = list(),  # accumulate messages
        msgnum = 0
    )
    # This line is the key to this approach
    lcl = list2env(lcl) # makes this list into an environment

    lcl$warn = function(msg=NULL, dostop=FALSE, inproc=FALSE) {
        # Accumulate messages and, if dostop or no message, display all
        # messages and end procedure state
        # If dostop, issue a stop.

        if (!is.null(msg)) { # accumulate message
            assign("msgnum", lcl$msgnum + 1, envir=lcl)
            # There seems to be no way to update an object, only replace it
            m = lcl$msglist
            m[[lcl$msgnum]] = msg
            assign("msglist", m, envir=lcl)
        } 

    if (is.null(msg) || dostop) {
        lcl$display(inproc)  # display messages and end procedure state
        if (dostop) {
            stop(gtxt("End of procedure"), call.=FALSE)  # may result in dangling error text
        }
    }
}

    lcl$display = function(inproc=FALSE) {
        # display any accumulated messages as a warnings table or as prints
        # and end procedure state, if any

    if (lcl$msgnum == 0) {   # nothing to display
        if (inproc) {
            spsspkg.EndProcedure()
        }
    } else {
        if (!inproc) {
            procok =tryCatch({
                StartProcedure(lcl$procname, lcl$omsid)
                TRUE
                },
                error = function(e) {
                    FALSE
                }
            )
        } else {
            procok = TRUE
        }
        if (procok) {  # build and display a Warnings table if we can
            table = spss.BasePivotTable("Warnings ","Warnings") # do not translate this
            rowdim = BasePivotTable.Append(table,Dimension.Place.row, 
                gtxt("Message Number"), hideName = FALSE,hideLabels = FALSE)

    for (i in 1:lcl$msgnum) {
        rowcategory = spss.CellText.String(as.character(i))
        BasePivotTable.SetCategories(table,rowdim,rowcategory)
        BasePivotTable.SetCellValue(table,rowcategory, 
            spss.CellText.String(lcl$msglist[[i]]))
    }
    spsspkg.EndProcedure()   # implies display
} else { # can't produce a table
    for (i in 1:lcl$msgnum) {
        print(lcl$msglist[[i]])
    }
}
}
}
return(lcl)
}

Run<-function(args){
    cmdname = args[[1]]
    args <- args[[2]]
    oobj<-spsspkg.Syntax(templ=list(
        spsspkg.Template("VARIABLE", subc="", ktype="varname", var="variable", islist=FALSE),
        # Next item is to catch case where dialog box cannot catch error.
        spsspkg.Template("COINTVARS", subc="", ktype="varname", var="falsecointvars", islist=TRUE),
        
        spsspkg.Template("COINTVARS", subc="PO", ktype="varname", var="cointvars", islist=TRUE),
        spsspkg.Template("INTERCEPT", subc="PO", ktype="bool", var="pointercept"),
        spsspkg.Template("TRUNCLAG", subc="PO", ktype="str", var="polag",
            vallist=list("long", "short")),
        
        spsspkg.Template("ALTHYPOT", subc="ADF", ktype="str", var="adfalth",
            vallist=list("stationary", "explosive")),
        spsspkg.Template("LAGORDER", subc="ADF", ktype="int", var="adflagorder"),
        
        spsspkg.Template("ALTHYPOT", subc="PP", ktype="str", var="ppalth", 
            vallist=list("stationary", "explosive")),
        spsspkg.Template("TYPE", subc="PP", ktype="str", var="pptype",
            vallist=list("zalpha", "ztalpha")),
        spsspkg.Template("TRUNCLAG", subc="PP", ktype="str", var="pplag",
            vallist=list("long", "short")),
        
        spsspkg.Template("NULLHYPOT", subc="KPSS", ktype="str", var="kpssnull",
            vallist=list("level", "trend")),
        spsspkg.Template("TRUNCLAG", subc="KPSS", ktype="str", "kpsslag",
            vallist=list("long", "short"))
    ))        
if ("HELP" %in% attr(args,"names"))
    helper(cmdname)
else
    res <- spsspkg.processcmd(oobj,args,"tstests")
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
    assign("helper", spsspkg.helper)
}
