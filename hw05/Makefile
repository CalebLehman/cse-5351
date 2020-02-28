all:
	mkdir -p build
	latex -output-directory=build -output-format=pdf solutions.tex
	latex -output-directory=build -output-format=pdf solutions.tex
	mv build/solutions.pdf solutions.pdf

.PHONY: clean
clean:
	rm -rf build solutions.pdf
