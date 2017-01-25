#' Conduct a noncompartmental analysis with CDISC dataset
#'
#' \code{runCDISC} uses ncar package to perform a noncompartmental analysis of
#' CDISC standardized pharmacokinetic dataset.
#'
#' @param wd Working directory of CDISC dataset, containing DM, EX and PC
#' @param filenameDM A filename of DM domain. Usually DM or dm
#' @param filenameEX A filename of EX domain. Usually EX or ex
#' @param filenamePC A filename of PC domain. Usually PC or pc
#' @param extension file extension, currently supporting only \code{.xpt}
#' @param incl_arm Vector of study arms of interest
#' @importFrom Hmisc sasxport.get
#' @importFrom measurements conv_unit
#' @importFrom lubridate ymd_hm
#' @importFrom utils packageDescription
#' @importFrom utils packageVersion
#' @import dplyr
#' @export
#' @return List of output data of noncompartmental analysis
#' @examples
#' #Currently there is no publicly open CDISC dataset for presenting an example.
runCDISC <- function(wd = getwd(), filenameDM = "DM", filenameEX = "EX", filenamePC = "PC",
                     extension = "xpt", incl_arm = NULL){

    # assign NULL - To suppress notes of R CMD check --as-CRAN
    ACTUALHR <- NULL
    ARM <- NULL
    EXDOSE <- NULL
    EXDOSECONV <- NULL
    EXDOSU <- NULL
    EXDOSUCONV <- NULL
    EXENDTC <- NULL
    EXSTDTC <- NULL
    INFUSIONHR <- NULL
    PCDTC <- NULL
    PCSPEC <- NULL
    PCSTRESN <- NULL
    PCSTRESU <- NULL
    USUBJID <- NULL

    # Actual function starts
    Output <- list()
    DM <- sasxport.get(paste0(wd, "/", filenameDM, ".", extension), as.is = TRUE)
    EX <- sasxport.get(paste0(wd, "/", filenameEX, ".", extension), as.is = TRUE)
    PC <- sasxport.get(paste0(wd, "/", filenamePC, ".", extension), as.is = TRUE)
    names(DM) <- toupper(names(DM))
    names(EX) <- toupper(names(EX))
    names(PC) <- toupper(names(PC))
    Output$EX <- EX
    if (is.null(incl_arm)){
        FocusSubj <- DM %>%
            select(USUBJID) %>% as.matrix()
    } else {
        FocusSubj <- DM %>%
            filter(ARM %in% incl_arm) %>%
            select(USUBJID) %>% as.matrix()
    }

    DosingData <- EX %>%
        filter(USUBJID %in% FocusSubj) %>%
        mutate(INFUSIONHR = as.numeric(difftime(ymd_hm(EXENDTC, tz = "Asia/Seoul"),
                                                ymd_hm(EXSTDTC, tz = "Asia/Seoul"),
                                                units = "hours"))) %>%
        filter(!is.na(INFUSIONHR)) %>%
        select(USUBJID, EXSTDTC, EXENDTC, INFUSIONHR, EXDOSE, EXDOSU) %>%
        filter(!duplicated(USUBJID))

    Output$ActualTimePlasma <- PC %>%
        filter(USUBJID %in% FocusSubj) %>%
        filter(PCSPEC == "PLASMA") %>%
        left_join(DosingData, by = "USUBJID") %>%
        mutate(ACTUALHR = as.numeric(difftime(ymd_hm(PCDTC, tz = "Asia/Seoul"),
                                              ymd_hm(EXSTDTC, tz = "Asia/Seoul"),
                                              units = "hours"))) %>%
        filter(!is.na(ACTUALHR)) %>%
        mutate(ACTUALHR = ifelse(ACTUALHR < 0, yes = 0, no = ACTUALHR)) %>%
        select(USUBJID, ACTUALHR, PCSTRESN, PCSTRESU, EXDOSE, EXDOSU, INFUSIONHR)

    Output$ActualTimePlasmaDose <- Output$ActualTimePlasma %>%
        filter(!duplicated(USUBJID)) %>%
        mutate(EXDOSECONV = conv_unit(EXDOSE,
                                      from = unique(EXDOSU),
                                      to = gsub("\\/.*$", "", unique(PCSTRESU))),
               EXDOSUCONV = gsub("\\/.*$", "", unique(PCSTRESU))) %>%
        select(USUBJID, EXDOSECONV, EXDOSUCONV, INFUSIONHR)

    Output$ResultNCA <- NCA(Data = Output$ActualTimePlasma, colSubj = "USUBJID",
                            colTime = "ACTUALHR", colConc = "PCSTRESN",
                            Dose = Output$ActualTimePlasmaDose[, "EXDOSECONV"], AdmMode = "Infusion",
                            TimeInfusion = mean(Output$ActualTimePlasmaDose[, "INFUSIONHR"]))

    return(Output)
}
