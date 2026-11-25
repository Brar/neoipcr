# The eight procedure groups, the placement of the recorded ICHI codes among
# them, and the classifier the reports and the validation rules share.
#
# A surgical procedure is grouped by the site its codes place it at and the
# tract it opens, as the protocol's review decided it; the recorded codes
# with their groups, tracts, approach codes and pending codes are the test
# cases of these rules. A code the table holds is placed as the table says;
# the axes of target, action and means below serve only a code it does not
# hold, and never contradict the table. An ICHI code expression is read as
# the intervention stems it is built of, and only the stems place; a value
# no grammar reads places nothing. A code no group takes stays out of
# every group: the warnings ask the partners what was done, and only the
# answer places it.

#' Group surgical procedures by the site their ICHI codes place them at
#'
#' @description
#' `get_procedure_group()` classifies a surgical procedure, given by its main
#' procedure code and its two side procedure codes, into one of the eight
#' procedure groups: the site the procedure is placed at, as the protocol's
#' review of the ICHI codes decided it. Each slot may hold a full ICHI code
#' expression, and every intervention stem of every slot counts among the
#' procedure's codes; a malformed value is a code no group takes. Every code
#' of a procedure counts:
#' the procedure joins the first group, in the fixed order of the group
#' levels, that any of its codes places, so the same procedure is grouped the
#' same way whichever of its codes was entered as the main one. An approach
#' code places a procedure only when it holds nothing but approach codes, and
#' a code that is no surgical procedure drops out. A procedure whose codes
#' place nothing — a valid code no rule places, a malformed value, or a code
#' whose query is still pending — is placed by no group; the warnings ask
#' what was done.
#'
#' @param main Character vector of main ICHI procedure codes.
#' @param side1 Character vector of first side ICHI procedure codes. `NA`
#'   stands for a slot that holds no code, and one `NA` covers a whole
#'   vector of procedures without a first side code.
#' @param side2 Character vector of second side ICHI procedure codes, with
#'   the same meaning. The arguments are recycled to a common length.
#' @returns A factor with the eight procedure groups as levels in their
#'   fixed order, plus `not_surgery` for a procedure all of whose codes are
#'   no surgical procedure, `no_group` for one its codes place nowhere, and
#'   `NA` for a procedure without any code.
#' @export
get_procedure_group <- function(main, side1 = NA_character_,
                                side2 = NA_character_) {
  n <- max(vapply(list(main, side1, side2), length, integer(1)))
  main <- rep_len(main, n)
  side1 <- rep_len(side1, n)
  side2 <- rep_len(side2, n)
  held <- rep_len(.procedure_holds_procedure(main, side1, side2), n)
  group <- .procedure_group_row(main, side1, side2)
  group[held] <- "no_group"
  group[is.na(main) & is.na(side1) & is.na(side2)] <- NA_character_
  factor(group,
         levels = c(.procedure_group_levels, "not_surgery", "no_group"))
}

# The groups in their order: a procedure whose several codes place it in
# more than one group joins the first of them in this order.
.procedure_group_levels <- c(
  "neurosurgery",
  "cardiac_great_vessel_surgery",
  "non_cardiac_thoracic_surgery",
  "abdominal_surgery",
  "abdominal_wall_surgery",
  "inguinal_umbilical_hernia_repair",
  "head_neck_upper_airway_surgery",
  "skin_soft_tissue_urogenital_surgery")

# The tracts whose opening raises a class I wound's risk. `none` covers a
# procedure that opens no tract; `unclear` covers a code whose description
# does not settle whether it does, and raises nothing.
.procedure_opening_tracts <- c("gastrointestinal", "respiratory", "urinary",
                               "genital")

# The shunt codes: beside one of them, `KMA.FA.AA` is the shunt's
# peritoneal end and its placement asks nothing; without one it is an
# operation on the peritoneum and the warning asks what was done.
.procedure_shunt_codes <- c("AAE.JB.AA", # Ventricular shunt to abdominal cavity and drain
                            "AAE.KA.AA", # Replacement of peritoneal end of ventricular shunt
                            "AAE.LH.AA", # Removal of ventricular shunt
                            "AAE.LI.AA", # Insertion of ventricular shunt
                            "AAE.LI.AB", # Laparoscopic insertion of ventricular shunt
                            "AAE.SN.AA") # Management of ventricular shunt

# The placement of every ICHI code the production data carries: its group,
# the tract it opens, whether it is an approach code, and — among the pending
# codes — whether it holds its whole procedure. A pending code's group is
# `NA`: it places nothing until the answer to its query; the classifier gives
# it the residual code no group takes.
.procedure_code_table <- local({
  chunks <- list(
    list("neurosurgery", "none", c(
      "AAA.JB.AA", # Tricuspid valvotomy with incision
      "AAE.DN.AA", # Resection of choroid plexus through incision
      "AAE.GA.AE", # External drainage of cerebrospinal fluid from head via ventricles
      "AAE.JB.AA", # Ventricular shunt to abdominal cavity and drain
      "AAE.JD.AA", # Insertion of ventricular catheter
      "AAE.KA.AA", # Replacement of peritoneal end of ventricular shunt
      "AAE.LH.AA", # Removal of ventricular shunt
      "AAE.LI.AA", # Insertion of ventricular shunt
      "AAE.LI.AB", # Laparoscopic insertion of ventricular shunt
      "AAE.SN.AA", # Management of ventricular shunt
      "AAG.DN.AA", # Evacuation of intracranial hematoma or hemorrhage through incision
      "ABA.FC.AA", # Closure of myelomeningocele and meninges with prosthesis
      "ABA.MK.AA", # Repair of meningocele of spinal cord
      "ABA.ML.AA", # Repair of myelomeningocele
      "ABG.LI.AA", # Placement of catheter into spinal canal for drainage
      "MAA.FA.AA"  # Craniotomy
    )),
    list("cardiac_great_vessel_surgery", "none", c(
      "HBB.JD.AA", # Insertion of prosthetic covering of heart using open approach
      "HIJ.LA.AA", # Ligation of patent ductus arteriosus
      "HIJ.LA.AB", # Laparoscopic ligation of patent ductus arteriosus
      "HIK.MK.AA"  # Repair of coarctation of aorta
    )),
    list("non_cardiac_thoracic_surgery", "none", c(
      "HIK.LJ.AA", # Aortopexy
      "JCA.JD.AA", # Insertion of prosthetic covering for pleura through incision
      "JCB.JB.AE", # Percutaneous drainage of pleural cavity
      "JCH.AE.AA", # Thoracoscopic surgery of thoracic cavity
      "JCH.FA.AA", # Thoracotomy
      "MCX.MK.AA", # Repair of diaphragmatic eventration or hernia
      "MCX.MK.AB"  # Laparoscopic repair of diaphragmatic eventration or hernia
    )),
    list("non_cardiac_thoracic_surgery", "respiratory", c(
      "JBA.MK.AA", # Repair of trachea
      "JBF.JJ.AA", # Lobectomy of lung
      "JBF.LL.AA"  # Thoracoscopic wedge resection of lung
    )),
    list("non_cardiac_thoracic_surgery", "gastrointestinal", c(
      # The oesophagus is placed in the chest.
      "KBA.LB.AA", # Mobilization of esophagus
      "KBA.MK.AA"  # Repair of esophageal atresia with or without fistula
    )),
    list("abdominal_surgery", "none", c(
      "DJA.MK.AA", # Repair of pyloric stenosis
      "IED.LA.AA", # Ligation of ductus venosus
      "KBZ.LD.AA", # Reduction of rectal prolapse
      "KMA.DN.AA", # Resection of peritoneum with excision and reanastomosis
      "KMA.FA.AA", # Creation of peritoneal (peritoneum) dialysis access
      "KMA.FC.AA", # Closure of peritoneostomy
      "KMA.JA.AA", # Aspiration of peritoneal cavity through incision
      "KMA.JB.AA", # Drainage of peritoneal cavity with incision
      "KMA.JJ.AA", # Biopsy of peritoneum with incision
      "KMA.KA.AA", # Replacement of peritoneal drainage device with incision
      "KMA.MK.AA", # Repair of umbilical vein
      "PAK.AE.AA", # Endoscopy of abdomen
      "PAK.AE.AB", # Laparoscopy
      "PAK.FA.AA", # Abdominal incision
      "PAK.FC.AA", # Closure of laparostomy
      "PAK.FC.AB", # Laparoscopic fascial closure
      "PAK.JB.AA", # Drainage of abdomen with incision
      "PAL.FA.AA"  # Incision of abdominal wall
    )),
    list("abdominal_surgery", "gastrointestinal", c(
      "KBF.AE.AB", # Laparotomy with gastroscopy
      "KBF.LI.AA", # Division of pyloric muscle
      "KBF.LI.AB", # Laparoscopic division of pyloric muscle
      "KBF.MK.AA", # Repair of gastric perforation
      "KBI.LB.AA", # Mobilization of duodenal atresia
      "KBI.MK.AA", # Repair of duodenal atresia with or without web
      "KBI.ML.AA", # Resection and anastomosis of duodenum
      "KBK.JI.AA", # Resection of intestine through incision
      "KBK.JJ.AA", # Enterotomy
      "KBK.JJ.AB", # Laparoscopic enterotomy
      "KBK.LB.AA", # Mobilization of intestine
      "KBK.LI.AA", # Lysis of intestinal adhesion
      "KBK.LI.AB", # Laparoscopic lysis of intestinal adhesion
      "KBK.MK.AA", # Repair of perforation of intestine, not elsewhere classified
      "KBK.MK.AB", # Laparoscopic repair of perforation of intestine, not elsewhere classified
      "KBO.JK.AA", # Appendicectomy
      "KBO.JK.AB", # Laparoscopic appendicectomy
      "KBP.FA.AB", # Laparoscopic mobilization of colon
      "KBP.JJ.AA", # Colotomy
      "KBP.LI.AA", # Lysis of adhesion of colon
      "KBP.MK.AA", # Repair of perforation of colon
      "KBW.JK.AA", # Resection of rectosigmoid colon with colostomy
      "KBZ.JJ.AA", # Incision of large intestine
      "KBZ.JJ.AB", # Laparoscopic incision of large intestine
      "KBZ.LB.AA", # Mobilization of rectum
      "KBZ.MK.AA", # Repair of large intestine, not elsewhere classified
      "KZZ.MK.AA", # Repair of intestine, not elsewhere classified
      "PTA.LA.AC", # Ligation of patent ductus venosus
      "PTB.LA.AC"  # Ligation of patent ductus venosus
    )),
    list("abdominal_surgery", "unclear", c(
      "KBF.FA.AA", # Exploration of stomach through incision
      "KBZ.LC.AA"  # Washout of colon
    )),
    list("abdominal_wall_surgery", "none", c(
      "PAK.LI.AA", # Release of laparostomy
      "PAL.MK.AA", # Suture of laceration of abdominal wall
      "PAL.MK.AB", # Laparoscopic repair of laceration of abdominal wall
      "PAL.ML.AA", # Resection of abdominal wall
      "PAO.JI.AA"  # Resection of omphalomesenteric (vitelline) duct
    )),
    list("inguinal_umbilical_hernia_repair", "none", c(
      "PAM.MK.AA", # Repair of inguinal hernia
      "PAM.MK.AB", # Laparoscopic repair of inguinal hernia
      "PAO.MK.AA", # Repair of umbilical hernia
      "PAO.MK.AB"  # Laparoscopic repair of umbilical hernia
    )),
    list("head_neck_upper_airway_surgery", "respiratory", c(
      "JAM.ML.AD", # Endoscopic resection of pharyngeal cyst or pharyngeal web
      "JAN.JI.AD", # Endoscopic excision of subglottic stenosis
      "JAN.MK.AD", # Endoscopic repair of larynx
      "JBA.LI.AA"  # Tracheostomy
    )),
    list("head_neck_upper_airway_surgery", "unclear", c(
      "KAA.AD.AA"  # Biopsy of lip
    )),
    list("head_neck_upper_airway_surgery", "gastrointestinal", c(
      # A frenotomy is an incision of the mucosa.
      "KAB.FB.AC"  # Lingual frenotomy
    )),
    list("skin_soft_tissue_urogenital_surgery", "none", c(
      "LAB.LL.AA", # Reduction of skin and subcutaneous cell tissue of trunk
      "LAG.JG.AA", # Debridement of skin and subcutaneous cell tissue of head, neck and cranial cavity
      "LCA.JG.AA", # Debridement of breast with incision
      "NGL.LC.AA", # Orchiopexy
      "PAW.JB.AA"  # Drainage of perineum
    )),
    list("skin_soft_tissue_urogenital_surgery", "urinary", c(
      "NAM.MK.AA"  # Repair of urethra
    )))
  outside <- c(
    # Imaging, whatever the anatomical target.
    "AAA.BA.BH", "ABA.BA.BH", "MBO.BA.BC", "PAB.BA.BH", "PAE.BA.BH",
    "PAK.BA.BH", "PZA.BA.BH",
    # Endoscopy without an incision of a tract.
    "JAN.AE.AC", "JBA.AE.AB", "JBB.AE.AD", "KBA.LG.AD", "KBI.AE.AD",
    # Treatment of retinopathy of prematurity.
    "BCC.GA.AA", "BCD.DB.AE",
    # Vascular access.
    "HIB.DL.AF", "IBD.DL.AF", "IDB.DL.AF", "IZD.DL.AF",
    # Transcatheter heart intervention.
    "HDG.LG.AF",
    # Needle punctures and percutaneous drains. Only the pleura's drainage
    # counts as a surgical procedure, and that one sits among the thoracic
    # codes above; the warning asks about the abdomen's two when the form
    # describes an open procedure.
    "JBF.JB.AE", "JBF.LL.AE", "KMA.JB.AE", "NAA.LI.AE", "PAK.JB.AE",
    # Device, stoma and wound care, and education.
    "JBA.KA.AC", "KBF.DL.AC", "KBF.KA.AC", "KBK.DL.AC", "KBK.LD.AH",
    "KBK.LG.AC", "LAB.JG.AH", "PTA.PM.ZZ", "PTB.SN.AC", "PTC.PM.ZZ")
  # Codes that name no operation that can be placed: what was done waits on
  # the answer to a query. The two held codes hold their whole procedure out
  # of every group until then, since no other code of such a procedure can
  # be trusted to be the operation; the other four place nothing and leave
  # the procedure to its codes that do place.
  pending <- c("BAA.BA.AD", "LZZ.DK.AH", "KZZ.AD.AA", "NMR.MK.AB",
               "NZZ.ZZ.ZZ", "KPB.LI.AA")
  pending_held <- c("BAA.BA.AD", "NMR.MK.AB")
  # Approach codes name the road taken and not the operation done, so they
  # place a procedure only when it holds nothing else that places.
  approach <- c("MAA.FA.AA", "JCH.AE.AA", "JCH.FA.AA", "PAK.AE.AA",
                "PAK.AE.AB", "PAK.FA.AA")

  group <- data.frame(
    procedure_code = unlist(lapply(chunks, \(ch) ch[[3]]), use.names = FALSE),
    group = unlist(lapply(chunks, \(ch)
      rep(ch[[1]], length(ch[[3]]))), use.names = FALSE),
    tract = unlist(lapply(chunks, \(ch)
      rep(ch[[2]], length(ch[[3]]))), use.names = FALSE),
    stringsAsFactors = FALSE)
  outside <- data.frame(
    procedure_code = outside, group = "not_surgery",
    tract = NA_character_, stringsAsFactors = FALSE)
  pending <- data.frame(
    procedure_code = pending, group = NA_character_,
    tract = NA_character_, stringsAsFactors = FALSE)
  table <- rbind(group, outside, pending)
  table$approach <- table$procedure_code %in% approach
  table$holds_procedure <- table$procedure_code %in% pending_held
  # Codes placed by a group yet whose placement the partners queried: the
  # peritoneal dialysis access counts as abdominal unless the procedure
  # holds a shunt code beside it, and the umbilical vein repair was recorded
  # only with an implant and is probably a patch closure. The warning asks
  # about the access whenever the procedure holds no shunt code, and about
  # the repair whatever else the procedure holds.
  table$conditional <- table$procedure_code %in% c("KMA.FA.AA", "KMA.MK.AA")
  table
})

# The axes of target, action and means that place a code the table does not
# hold. A recorded code is never asked: the table's placement of it stands.
# A valid code no rule places, and a value that is no ICHI code at all, fall
# to `NA`: the code no group takes.
.procedure_group_by_axis <- function(x) {
  target <- stringr::str_extract(x, "^([A-Za-z]{3})\\.", 1)
  action <- stringr::str_extract(x, "^([A-Za-z]{3})\\.([A-Za-z]{2})", 2)
  means <- stringr::str_extract(x, "^([A-Za-z]{3})\\.([A-Za-z]{2})\\.([A-Za-z]{2})", 3)

  dplyr::case_when(
    # Neurosurgery: interventions on the brain, its ventricles and
    # coverings, the spinal cord and canal, and the skull, by an incision,
    # an excision or a percutaneous analytic approach.
    target %in% c(
      "AAA", # Interventions on brain, multiple sites
      "AAE", # Interventions on ventricles of brain
      "AAG", # Interventions on intracranial space
      "ABA", # Interventions on spinal cord
      "ABG", # Interventions on spinal canal
      "MAA"  # Interventions on skull
      ) &
      means %in% c("AA", "AB", "AE") ~ "neurosurgery",

    # Cardiac and great-vessel surgery. Vascular catheters and ports, and
    # transcatheter heart interventions, never count, so the axis admits the
    # open and endoscopic means only — never the percutaneous one.
    target == "HIJ" & action == "LA" & means %in% c("AA", "AB") ~
      "cardiac_great_vessel_surgery",
    target == "HIK" & action == "MK" & means == "AA" ~
      "cardiac_great_vessel_surgery",

    # Non-cardiac thoracic surgery. The percutaneous drainage of the pleural
    # cavity counts as surgery wherever it is coded, so the pleura's means
    # admit the percutaneous approach where no other thoracic target does.
    target %in% c(
      "MCX", # Interventions on diaphragm
      "JBF", # Interventions on lung parenchyma
      "JCA", # Interventions on pleura, visceral
      "JCH"  # Interventions on thoracic cavity
      ) &
      means %in% c("AA", "AB") ~ "non_cardiac_thoracic_surgery",
    target == "JCB" &
      means %in% c("AA", "AB", "AE") ~ "non_cardiac_thoracic_surgery",

    # The oesophagus is placed in the chest.
    target == "KBA" & means %in% c("AA", "AB") ~
      "non_cardiac_thoracic_surgery",

    # Abdominal surgery, the abdominal wall's and the hernia's repairs
    # aside; the shunt's distal ends follow their target's means.
    target %in% c(
      "KBF", # Interventions on stomach
      "KBI", # Interventions on duodenum
      "KBK", # Interventions on small intestine, not elsewhere classified
      "KBO", # Interventions on appendix
      "KBP", # Interventions on colon
      "KBZ", # Interventions on large intestine, not elsewhere classified
      "KMA", # Interventions on peritoneum
      "PAK"  # Interventions on abdomen, not otherwise specified
      ) &
      means %in% c("AA", "AB") ~ "abdominal_surgery",
    target %in% c("PTA", "PTB") & action == "LA" & means == "AC" ~
      "abdominal_surgery",

    # Inguinal and umbilical hernia repair.
    target == "PAM" & means %in% c("AA", "AB") ~
      "inguinal_umbilical_hernia_repair",

    .default = NA_character_)
}

# The intervention stems of one ICHI code expression. The grammar composes
# an expression of a stem with extension clauses and other-code suffixes,
# and joins the stems of interventions done together with "/"; only stems
# classify. An expression the grammar rejects yields no stem: a malformed
# value places nothing.
.procedure_stems_of_one <- function(code) {
  if (is.na(code) || !is_valid_ichi_code(code)) return(character(0))
  parts <- strsplit(code, "/", fixed = TRUE)[[1]]
  sub("[&#].*$", "", parts)
}

# The group each stem places by itself: the table for a stem it holds, the
# axes for one it does not, and the code no group takes for one neither
# places. `not_surgery` codes and pending alike pass through their lookup.
.procedure_group_for_stem <- function(stems) {
  idx <- match(stems, .procedure_code_table$procedure_code)
  in_table <- !is.na(idx)
  table_group <- .procedure_code_table$group[idx]
  axis <- .procedure_group_by_axis(stems)
  group <- dplyr::case_when(
    is.na(stems) ~ NA_character_,
    in_table & !is.na(table_group) ~ table_group,
    in_table ~ "no_group",
    !is.na(axis) ~ axis,
    .default = "no_group")
  group
}

# The group the given stems of one procedure place: the earliest group of a
# stem that is no approach code, an approach's group when nothing else
# places, and no surgical procedure when every stem is.
.procedure_group_of_stems <- function(stems) {
  idx <- match(stems, .procedure_code_table$procedure_code)
  recorded <- !is.na(idx)
  groups <- .procedure_group_for_stem(stems)
  placed <- groups %in% .procedure_group_levels
  is_approach <- logical(length(stems))
  is_approach[recorded] <- .procedure_code_table$approach[idx[recorded]]
  non_approach <- placed & !is_approach
  if (any(non_approach)) {
    return(.procedure_first_group(groups[non_approach]))
  }
  if (any(placed)) return(.procedure_first_group(groups[placed]))
  # A procedure is no surgical procedure only when every one of its codes
  # says so: a code no group takes among them asks what was done, and the
  # answer could still place the procedure.
  if (all(groups == "not_surgery")) return("not_surgery")
  "no_group"
}

get_procedure_category <- function(x, not_surgery_na = FALSE) {
  group <- vapply(as.character(x), function(one) {
    if (is.na(one)) return(NA_character_)
    stems <- .procedure_stems_of_one(one)
    # A value no grammar reads — a malformed code — places nothing.
    if (length(stems) == 0L) return("no_group")
    .procedure_group_of_stems(stems)
  }, character(1), USE.NAMES = FALSE)
  if (not_surgery_na) {
    # The surgical counts hold only a code a group takes: a code that is no
    # surgical procedure and a code no group takes alike read as missing.
    group[group %in% c("not_surgery", "no_group")] <- NA_character_
  }
  levels <- c(.procedure_group_levels,
              if (!not_surgery_na) c("not_surgery", "no_group"))
   factor(group, levels = levels)
 }

# Whether any of a procedure's codes holds the whole procedure out of every
# group: the held pending codes, which name no operation the other codes of
# their procedure can be trusted beside.
.procedure_holds_procedure <- function(main, side1, side2) {
  holds <- function(codes) {
    stems <- unlist(lapply(codes, .procedure_stems_of_one), use.names = FALSE)
    idx <- match(stems, .procedure_code_table$procedure_code)
    recorded <- which(!is.na(idx))
    length(recorded) > 0L &&
      any(.procedure_code_table$holds_procedure[idx[recorded]])
  }
  vapply(seq_along(main),
         \(i) holds(c(main[[i]], side1[[i]], side2[[i]])), logical(1))
}

# The group one procedure joins: the earliest group a code of its own
# operation places, an approach's group only when nothing else places, and
# the code that is no surgical procedure's own code when all its codes are.
# Each code slot holds a full ICHI code expression; its stems count among
# the procedure's codes, and a slot that is no valid expression places
# nothing yet marks the procedure as recorded.
.procedure_group_row <- function(main, side1, side2) {
  row <- function(m, s1, s2) {
    fields <- c(m, s1, s2)
    if (all(is.na(fields))) return(NA_character_)
    stems <- unlist(lapply(fields, .procedure_stems_of_one),
                    use.names = FALSE)
    # A slot was filled and no grammar reads it: a code no group takes.
    if (length(stems) == 0L) return("no_group")
    .procedure_group_of_stems(stems)
  }
  mapply(row, main, side1, side2, USE.NAMES = FALSE)
}

# The earliest of the given groups in the order of `.procedure_group_levels`.
.procedure_first_group <- function(groups) {
  groups[[which.min(match(groups, .procedure_group_levels))]]
}

# The risk stratum of a procedure within its group: its wound class, and —
# within class I — whether a code of the procedure's own group opens a tract
# and whether an implant was inserted or the wound closed primarily.
.procedure_risk_stratum <- function(main, side1, side2, wound_class,
                                    implant, primary_closure) {
  n <- max(vapply(list(main, side1, side2, wound_class, implant,
                       primary_closure), length, integer(1)))
  main <- rep_len(main, n)
  side1 <- rep_len(side1, n)
  side2 <- rep_len(side2, n)
  wound <- as.character(rep_len(wound_class, n))
  implant <- rep_len(implant, n)
  closure <- rep_len(primary_closure, n)
  group <- as.character(get_procedure_group(main, side1, side2))

  opens_tract <- vapply(seq_len(n), function(i) {
    g <- group[[i]]
    if (is.na(g) || !(g %in% .procedure_group_levels)) return(FALSE)
    stems <- unlist(lapply(c(main[[i]], side1[[i]], side2[[i]]),
                           .procedure_stems_of_one), use.names = FALSE)
    if (length(stems) == 0L) return(FALSE)
    idx <- match(stems, .procedure_code_table$procedure_code)
    recorded <- !is.na(idx)
    # Only a code of the procedure's own group opens its tract: an
    # unrecorded code's group by the axes counts too, and its tract, which
    # nothing records, raises nothing.
    code_group <- .procedure_group_for_stem(stems)
    own <- code_group == g
    tract <- rep(NA_character_, length(stems))
    tract[recorded] <- .procedure_code_table$tract[idx[recorded]]
    any(own & !is.na(tract) & tract %in% .procedure_opening_tracts)
  }, logical(1))

  # Both clean strata are closed primarily and open no tract: an implant
  # makes the one, an explicitly absent implant the other, and a class I
  # wound that fails either test counts in its group and in no stratum.
  stratum <- dplyr::case_when(
    is.na(group) | !(group %in% .procedure_group_levels) ~ NA_character_,
    is.na(wound) ~ NA_character_,
    wound %in% c("3", "4") ~ "contaminated_or_dirty",
    wound == "2" ~ "clean_contaminated",
    wound == "1" & opens_tract ~ "clean_contaminated",
    wound == "1" & !is.na(closure) & closure &
      !is.na(implant) & implant ~ "clean_with_implant",
    wound == "1" & !is.na(closure) & closure &
      !is.na(implant) & !implant ~ "clean",
    .default = NA_character_)
  factor(stratum,
         levels = c("clean", "clean_with_implant", "clean_contaminated",
                    "contaminated_or_dirty"))
}

get_procedure_category_pretty <- function(x) {
  dplyr::recode_values(
    as.character(x),
    "overall" ~ gettext("Overall"),
    "neurosurgery" ~ gettext("Neurosurgery"),
    "cardiac_great_vessel_surgery" ~
      gettext("Cardiac and great-vessel surgery"),
    "non_cardiac_thoracic_surgery" ~ gettext("Non-cardiac thoracic surgery"),
    "abdominal_surgery" ~ gettext("Abdominal surgery"),
    "abdominal_wall_surgery" ~ gettext("Abdominal wall surgery"),
    "inguinal_umbilical_hernia_repair" ~
      gettext("Inguinal and umbilical hernia repair"),
    "head_neck_upper_airway_surgery" ~
      gettext("Head, neck, and upper airway surgery"),
    "skin_soft_tissue_urogenital_surgery" ~
      gettext("Skin, soft tissue, perineal, and urogenital surgery"),
    "not_surgery" ~ gettext("Not a surgical procedure"),
    "no_group" ~ gettext("Placed by no procedure group"),
    # NEOIPC-PERMANENT(dataset-format): never remove the following lines.
    # This function renders whatever category vector it is handed, and such
    # a vector reaches it inside a dataset serialized before the groups
    # stood; a file on disk outlives the code that wrote it, so no condition
    # retires these. Without them those rows render as their bare code
    # instead of a label.
    "cardiac_and_large_vessel_surgery" ~ gettext("Cardiac- / large vessel surgery"),
    "lung_pleural_space_thoracic_surgery" ~ gettext("Lung- / pleural space- / thoracic surgery"),
    "oesophageal_surgery" ~ gettext("Oesophageal surgery"),
    "inguinal_hernia_surgery" ~ gettext("Inguinal hernia surgery"),
    "other" ~ gettext("Other"),
    "to_be_categorized" ~ gettext("Not yet categorized"),
    "to_be_categorised" ~ gettext("Not yet categorized"),
    default = x
  )
}

get_procedure_categories <- function(
    x, pretty = FALSE, include_iche = FALSE, use_cache = TRUE) {
  cache_key <- paste0("procedure_categories.v",
                      .surgical_definitions_version())

  # ToDo: Clarify licensing and inclusion criteria for ICHE information with WHO
  # and add ICHE table
  # if(include_iche)
  #   cache_key <- paste0(cache_key, ".iche")

  if(pretty)
  {
    l <- Sys.getenv("LANGUAGE")
    if(l == "")
      l <- Sys.getlocale("LC_MESSAGES")
    if(l == "")
      pretty <- FALSE # just in case
    else
      cache_key <- paste0(cache_key, ".", l)
  }

  if(use_cache && !is.null(r <- get_cached(x, cache_key)))
    return(r)

  r <- tibble::tibble(
    procedure_code = c(
      x$surgeryData$main_procedure_code,
      x$surgeryData$side_procedure_code_1,
      x$surgeryData$side_procedure_code_2) |>
      unique() |>
      sort() |>
      as.character()) |>
    dplyr::mutate(pro_cat = get_procedure_category(.data$procedure_code))

  if(pretty)
  {

    with_pretty <- r |>
      dplyr::select("pro_cat") |>
      dplyr::mutate(
        pretty_name = get_procedure_category_pretty(.data$pro_cat))

    pairs <- with_pretty |> dplyr::distinct()

    col_names <- stats::setNames(
      gettext("Procedure code","Procedure category"),
      c("procedure_code","pro_cat"))
    row_names <- stats::setNames(pairs$pretty_name, pairs$pro_cat)

    attr(r, "names.pretty") <- col_names
    attr(r, "row.names.pretty") <- row_names

    r <- r |>
    dplyr::mutate(
      pro_cat = with_pretty$pretty_name) |>
    dplyr::rename(
      !!col_names[["procedure_code"]] := .data$procedure_code,
      !!col_names[["pro_cat"]] := .data$pro_cat)
  }

  # if(include_iche)
  #   r <- r |>
  #   dplyr::inner_join(
  #     ichi_health_interventions,join_by(main_procedure_code == code))

  r |>
    cache(x, cache_key)
}
