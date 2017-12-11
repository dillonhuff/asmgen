# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.5

# Default target executed when no arguments are given to make.
default_target: all

.PHONY : default_target

# Allow only one "make -f Makefile2" at a time, but pass parallelism.
.NOTPARALLEL:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /opt/local/bin/cmake

# The command to remove a file.
RM = /opt/local/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/dillon/CppWorkspace/asmgen

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/dillon/CppWorkspace/asmgen

#=============================================================================
# Targets provided globally by CMake.

# Special rule for the target edit_cache
edit_cache:
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --cyan "Running CMake cache editor..."
	/opt/local/bin/ccmake -H$(CMAKE_SOURCE_DIR) -B$(CMAKE_BINARY_DIR)
.PHONY : edit_cache

# Special rule for the target edit_cache
edit_cache/fast: edit_cache

.PHONY : edit_cache/fast

# Special rule for the target rebuild_cache
rebuild_cache:
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --cyan "Running CMake to regenerate build system..."
	/opt/local/bin/cmake -H$(CMAKE_SOURCE_DIR) -B$(CMAKE_BINARY_DIR)
.PHONY : rebuild_cache

# Special rule for the target rebuild_cache
rebuild_cache/fast: rebuild_cache

.PHONY : rebuild_cache/fast

# The main all target
all: cmake_check_build_system
	$(CMAKE_COMMAND) -E cmake_progress_start /Users/dillon/CppWorkspace/asmgen/CMakeFiles /Users/dillon/CppWorkspace/asmgen/CMakeFiles/progress.marks
	$(MAKE) -f CMakeFiles/Makefile2 all
	$(CMAKE_COMMAND) -E cmake_progress_start /Users/dillon/CppWorkspace/asmgen/CMakeFiles 0
.PHONY : all

# The main clean target
clean:
	$(MAKE) -f CMakeFiles/Makefile2 clean
.PHONY : clean

# The main clean target
clean/fast: clean

.PHONY : clean/fast

# Prepare targets for installation.
preinstall: all
	$(MAKE) -f CMakeFiles/Makefile2 preinstall
.PHONY : preinstall

# Prepare targets for installation.
preinstall/fast:
	$(MAKE) -f CMakeFiles/Makefile2 preinstall
.PHONY : preinstall/fast

# clear depends
depend:
	$(CMAKE_COMMAND) -H$(CMAKE_SOURCE_DIR) -B$(CMAKE_BINARY_DIR) --check-build-system CMakeFiles/Makefile.cmake 1
.PHONY : depend

#=============================================================================
# Target rules for targets named all-tests

# Build rule for target.
all-tests: cmake_check_build_system
	$(MAKE) -f CMakeFiles/Makefile2 all-tests
.PHONY : all-tests

# fast build rule for target.
all-tests/fast:
	$(MAKE) -f CMakeFiles/all-tests.dir/build.make CMakeFiles/all-tests.dir/build
.PHONY : all-tests/fast

src/DataGraph.o: src/DataGraph.cpp.o

.PHONY : src/DataGraph.o

# target to build an object file
src/DataGraph.cpp.o:
	$(MAKE) -f CMakeFiles/all-tests.dir/build.make CMakeFiles/all-tests.dir/src/DataGraph.cpp.o
.PHONY : src/DataGraph.cpp.o

src/DataGraph.i: src/DataGraph.cpp.i

.PHONY : src/DataGraph.i

# target to preprocess a source file
src/DataGraph.cpp.i:
	$(MAKE) -f CMakeFiles/all-tests.dir/build.make CMakeFiles/all-tests.dir/src/DataGraph.cpp.i
.PHONY : src/DataGraph.cpp.i

src/DataGraph.s: src/DataGraph.cpp.s

.PHONY : src/DataGraph.s

# target to generate assembly for a file
src/DataGraph.cpp.s:
	$(MAKE) -f CMakeFiles/all-tests.dir/build.make CMakeFiles/all-tests.dir/src/DataGraph.cpp.s
.PHONY : src/DataGraph.cpp.s

src/LowProgram.o: src/LowProgram.cpp.o

.PHONY : src/LowProgram.o

# target to build an object file
src/LowProgram.cpp.o:
	$(MAKE) -f CMakeFiles/all-tests.dir/build.make CMakeFiles/all-tests.dir/src/LowProgram.cpp.o
.PHONY : src/LowProgram.cpp.o

src/LowProgram.i: src/LowProgram.cpp.i

.PHONY : src/LowProgram.i

# target to preprocess a source file
src/LowProgram.cpp.i:
	$(MAKE) -f CMakeFiles/all-tests.dir/build.make CMakeFiles/all-tests.dir/src/LowProgram.cpp.i
.PHONY : src/LowProgram.cpp.i

src/LowProgram.s: src/LowProgram.cpp.s

.PHONY : src/LowProgram.s

# target to generate assembly for a file
src/LowProgram.cpp.s:
	$(MAKE) -f CMakeFiles/all-tests.dir/build.make CMakeFiles/all-tests.dir/src/LowProgram.cpp.s
.PHONY : src/LowProgram.cpp.s

src/Output.o: src/Output.cpp.o

.PHONY : src/Output.o

# target to build an object file
src/Output.cpp.o:
	$(MAKE) -f CMakeFiles/all-tests.dir/build.make CMakeFiles/all-tests.dir/src/Output.cpp.o
.PHONY : src/Output.cpp.o

src/Output.i: src/Output.cpp.i

.PHONY : src/Output.i

# target to preprocess a source file
src/Output.cpp.i:
	$(MAKE) -f CMakeFiles/all-tests.dir/build.make CMakeFiles/all-tests.dir/src/Output.cpp.i
.PHONY : src/Output.cpp.i

src/Output.s: src/Output.cpp.s

.PHONY : src/Output.s

# target to generate assembly for a file
src/Output.cpp.s:
	$(MAKE) -f CMakeFiles/all-tests.dir/build.make CMakeFiles/all-tests.dir/src/Output.cpp.s
.PHONY : src/Output.cpp.s

src/RegisterAssignment.o: src/RegisterAssignment.cpp.o

.PHONY : src/RegisterAssignment.o

# target to build an object file
src/RegisterAssignment.cpp.o:
	$(MAKE) -f CMakeFiles/all-tests.dir/build.make CMakeFiles/all-tests.dir/src/RegisterAssignment.cpp.o
.PHONY : src/RegisterAssignment.cpp.o

src/RegisterAssignment.i: src/RegisterAssignment.cpp.i

.PHONY : src/RegisterAssignment.i

# target to preprocess a source file
src/RegisterAssignment.cpp.i:
	$(MAKE) -f CMakeFiles/all-tests.dir/build.make CMakeFiles/all-tests.dir/src/RegisterAssignment.cpp.i
.PHONY : src/RegisterAssignment.cpp.i

src/RegisterAssignment.s: src/RegisterAssignment.cpp.s

.PHONY : src/RegisterAssignment.s

# target to generate assembly for a file
src/RegisterAssignment.cpp.s:
	$(MAKE) -f CMakeFiles/all-tests.dir/build.make CMakeFiles/all-tests.dir/src/RegisterAssignment.cpp.s
.PHONY : src/RegisterAssignment.cpp.s

test/main.o: test/main.cpp.o

.PHONY : test/main.o

# target to build an object file
test/main.cpp.o:
	$(MAKE) -f CMakeFiles/all-tests.dir/build.make CMakeFiles/all-tests.dir/test/main.cpp.o
.PHONY : test/main.cpp.o

test/main.i: test/main.cpp.i

.PHONY : test/main.i

# target to preprocess a source file
test/main.cpp.i:
	$(MAKE) -f CMakeFiles/all-tests.dir/build.make CMakeFiles/all-tests.dir/test/main.cpp.i
.PHONY : test/main.cpp.i

test/main.s: test/main.cpp.s

.PHONY : test/main.s

# target to generate assembly for a file
test/main.cpp.s:
	$(MAKE) -f CMakeFiles/all-tests.dir/build.make CMakeFiles/all-tests.dir/test/main.cpp.s
.PHONY : test/main.cpp.s

# Help Target
help:
	@echo "The following are some of the valid targets for this Makefile:"
	@echo "... all (the default if no target is provided)"
	@echo "... clean"
	@echo "... depend"
	@echo "... edit_cache"
	@echo "... rebuild_cache"
	@echo "... all-tests"
	@echo "... src/DataGraph.o"
	@echo "... src/DataGraph.i"
	@echo "... src/DataGraph.s"
	@echo "... src/LowProgram.o"
	@echo "... src/LowProgram.i"
	@echo "... src/LowProgram.s"
	@echo "... src/Output.o"
	@echo "... src/Output.i"
	@echo "... src/Output.s"
	@echo "... src/RegisterAssignment.o"
	@echo "... src/RegisterAssignment.i"
	@echo "... src/RegisterAssignment.s"
	@echo "... test/main.o"
	@echo "... test/main.i"
	@echo "... test/main.s"
.PHONY : help



#=============================================================================
# Special targets to cleanup operation of make.

# Special rule to run CMake to check the build system integrity.
# No rule that depends on this can have commands that come from listfiles
# because they might be regenerated.
cmake_check_build_system:
	$(CMAKE_COMMAND) -H$(CMAKE_SOURCE_DIR) -B$(CMAKE_BINARY_DIR) --check-build-system CMakeFiles/Makefile.cmake 0
.PHONY : cmake_check_build_system

