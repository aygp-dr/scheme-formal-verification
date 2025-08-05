# Contributing to Guile Scheme Formal Verification Toolkit

Thank you for your interest in contributing to the Guile Scheme Formal Verification Toolkit! This document provides guidelines for contributing to the project.

## Quick Start

1. **Check dependencies**: Run `make check-deps` to verify your system setup
2. **Run examples**: Execute `make verify-examples` to ensure everything works
3. **Test changes**: Use `make test` to run the test suite (when available)

## Development Setup

### Prerequisites

- Guile 2.2.7+ (Guile 3.0+ recommended)
- Standard SRFI modules (srfi-1, srfi-26, srfi-64)
- Git for version control

### Project Structure

```
├── src/verification/          # Core verification modules
├── examples/                  # Working demonstration examples
├── specs/                     # TLA+ specification samples
├── experiments/               # Experimental code and tests
├── research/                  # Research artifacts (git-ignored)
├── Makefile                   # Build and test automation
├── README.org                 # Project documentation
└── LICENSE                    # MIT license
```

## Contribution Guidelines

### Code Style

- Follow standard Scheme conventions
- Use descriptive variable and function names  
- Include docstrings for public functions
- Maintain compatibility with Guile 2.2+

### Module Structure

All modules should:
- Include proper module declarations with #:use-module and #:export
- Provide comprehensive error handling
- Include self-tests when loaded interactively
- Document all exported functions

### Testing

- All new features must include working examples
- Examples should demonstrate real-world usage
- Property-based tests are preferred where applicable
- Ensure cross-platform compatibility

### Git Workflow

- Use conventional commit messages (feat:, fix:, docs:, test:, chore:)
- Include Co-Authored-By trailers when appropriate
- Write descriptive commit messages focusing on "why" not just "what"
- Create focused commits that address single concerns

### Pull Request Process

1. Fork the repository
2. Create a feature branch from `main`
3. Make your changes with appropriate tests
4. Ensure `make verify-examples` passes
5. Update documentation as needed
6. Submit pull request with clear description

## Areas for Contribution

### Current Priorities (0.1.0+)

- **Property Testing**: Enhance generators and shrinking strategies
- **TLA+ Integration**: Improve parsing and Scheme code generation
- **Documentation**: Add more examples and tutorials
- **Performance**: Optimize core verification routines

### Future Opportunities

- **Model Checking**: Implement state space exploration
- **Theorem Proving**: Add dependent type support
- **Tool Integration**: Connect with external verifiers
- **Educational Resources**: Create formal methods tutorials

## Code Review

All contributions go through code review focusing on:

- **Correctness**: Does the code work as intended?
- **Clarity**: Is the code readable and well-documented?
- **Compatibility**: Does it work across supported Guile versions?
- **Testing**: Are there adequate examples and tests?

## Getting Help

- **Issues**: Use GitHub issues for bug reports and feature requests
- **Discussions**: Start discussions for design questions
- **Examples**: Look at existing examples for patterns and style

## License

By contributing, you agree that your contributions will be licensed under the MIT License.

## Recognition

Contributors will be acknowledged in:
- Git commit Co-Authored-By trailers
- Project documentation
- Release notes for significant contributions

---

*This is a living document that evolves with the project. Suggestions for improvements are welcome!*