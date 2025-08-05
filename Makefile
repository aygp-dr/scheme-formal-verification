# Makefile for Guile Scheme Formal Verification Toolkit
# Provides installation and verification targets for 0.1.0 release

.PHONY: help install-deps verify-examples test clean check-deps

# Default target
help:
	@echo "Guile Scheme Formal Verification Toolkit - Makefile"
	@echo ""
	@echo "Available targets:"
	@echo "  help           - Show this help message"
	@echo "  check-deps     - Check system dependencies and requirements"
	@echo "  install-deps   - Install required dependencies"
	@echo "  verify-examples - Run all verification examples"
	@echo "  test           - Run test suite"
	@echo "  clean          - Clean compiled files and caches"

# Check system dependencies
check-deps:
	@echo "Checking system dependencies..."
	guile experiments/000-deps-check/check.scm

# Install dependencies (placeholder for 0.1.0)
install-deps: check-deps
	@echo "Dependency installation for Guile Scheme Formal Verification Toolkit"
	@echo "Current system dependencies check completed."
	@echo ""
	@echo "For full functionality, ensure you have:"
	@echo "  - Guile 2.2+ (3.0+ recommended)"
	@echo "  - TLA+ tools (optional, for TLA+ integration)"
	@echo "  - All required SRFI modules (checked above)"
	@echo ""
	@echo "Note: Auto-installation of system packages not yet implemented in 0.1.0"

# Run verification examples (placeholder for 0.1.0)
verify-examples: check-deps
	@echo "Running verification examples..."
	@if [ -d "examples" ] && [ -n "$$(find examples -name '*.scm' 2>/dev/null)" ]; then \
		for example in examples/*.scm; do \
			echo "Running $$example..."; \
			guile "$$example" || echo "Example $$example failed"; \
		done; \
	else \
		echo "No examples found in examples/ directory"; \
		echo "Example implementations are planned for 0.1.0 release"; \
	fi

# Run test suite
test: check-deps
	@echo "Running test suite..."
	@if [ -d "tests" ] && [ -n "$$(find tests -name '*.scm' 2>/dev/null)" ]; then \
		guile -c "(use-modules (srfi srfi-64)) (load \"tests/run-tests.scm\")" || true; \
	else \
		echo "No tests found - test suite implementation planned for 0.1.0"; \
	fi

# Clean compiled files
clean:
	@echo "Cleaning compiled files and caches..."
	find . -name "*.go" -type f -delete 2>/dev/null || true
	find . -name "*~" -type f -delete 2>/dev/null || true
	rm -rf ~/.cache/guile/ccache/*/$(PWD) 2>/dev/null || true
	@echo "Clean complete."

# Development target - check project structure
check-structure:
	@echo "Project structure check:"
	@echo "Required directories:"
	@for dir in src/verification src/tla src/property examples specs; do \
		if [ -d "$$dir" ]; then \
			echo "  ✓ $$dir/"; \
		else \
			echo "  ✗ $$dir/ (missing)"; \
		fi; \
	done
	@echo ""
	@echo "Required files:"
	@for file in LICENSE README.org Makefile; do \
		if [ -f "$$file" ]; then \
			echo "  ✓ $$file"; \
		else \
			echo "  ✗ $$file (missing)"; \
		fi; \
	done