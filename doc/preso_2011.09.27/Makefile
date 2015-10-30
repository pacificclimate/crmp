FIGURES = data_import_and_qc_landscape.pdf qc_persist_poor_precision.pdf temp_precision.pdf persistence_good.pdf persistence_boxplot.pdf threshold_success.pdf thresh_fail_ts.pdf thresh_fail_boxplot.pdf thresh_bad_units.pdf thresh_bad_units_corrected.pdf replace_na.pdf summer_zeros_success.pdf summer_zeros_partial_success.pdf summer_zeros_fail.pdf data_import_only.pdf archive_and_xmit_only.pdf
IMAGES = Rlogo.jpg opendap_logo_masthead.gif debian.png pcic_header3.png uvic.jpg lyx_banner.png
SLIDES = qc_preso
NOTES  = qc_preso_notes

$(SLIDES).pdf: $(SLIDES).tex $(FIGURES) $(IMAGES)
	pdflatex $(SLIDES)

# Use this if you're using lyx 1.6.4 which only has the beamer-article class
# This converts a beamer-article.tex to a beamer-presentation.tex
#rcs_slides.tex: rcs_howto.tex
#	cat rcs_howto.tex | sed -e "s/\\documentclass\[english\]{article}/\\documentclass\[english\]{beamer}/" |\
#	                    grep -v "\\usepackage\[letterpaper\]{geometry}"> rcs_slides.tex

notes: $(NOTES).pdf

$(NOTES).pdf: $(NOTES).tex
	pdflatex $(NOTES)

$(NOTES).tex: $(SLIDES).tex
	cat $(SLIDES).tex | sed -e "s/\\documentclass\[english\]/\\documentclass\[english,notes\]/" > $(NOTES).tex

$(SLIDES).tex: $(SLIDES).lyx
	rm -f $(SLIDES).tex ; lyx -e "latex" $(SLIDES).lyx

%.pdf: %.eps
	a2ping $< $@

%.eps: %.dia
	dia -e $@ $<

clean:
	rm *.log *~ *.aux *.toc *.nav *.out *.snm
