SRCDIR := $(PWD)/code
BUILDDIR := $(PWD)/.build
OUTDIR := $(PWD)/target

PROVE := gnatprove

THESIS := accountmanagementsystem.adb

MAINSRC := $(wildcard $(SRCDIR)/*.ads) \
    $(wildcard $(SRCDIR)/*.adb) \
    $(wildcard $(SRCDIR)/*.gpr)
MAINDEPS := $(MAINSRC:$(SRCDIR)/%=$(BUILDDIR)/%)

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(OUTDIR):
	mkdir $(OUTDIR)

$(BUILDDIR)/%: $(SRCDIR)/%
	ln -s $< $@

prove: | $(BUILDDIR) $(MAINDEPS)
	cd $(BUILDDIR) && gnatprove -P default -u $(THESIS)

.PHONY: prove clean

clean:
	rm -rf $(BUILDDIR) $(OUTDIR)
