python=python
pip=pip

all:
	@echo ""
	@echo " Targets:"
	@echo ""
	@echo " - test"
	@echo ""

test:
	nosetests -sv
