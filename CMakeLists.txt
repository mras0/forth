cmake_minimum_required(VERSION 3.7)
project(forth C ASM)

if (MSVC)
    set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Supported build configurations" FORCE)

    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /Zi /Zo /sdl /W4")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /DEBUG")
    add_definitions("-D_CRT_SECURE_NO_WARNINGS")
else()
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -ggdb -Wall -Wextra -pedantic -std=c99")
endif()

add_executable(cforth cforth.c)

if (${CMAKE_SYSTEM_PROCESSOR} STREQUAL arm)
    add_executable(forth asmdriver.c forth-arm.S)
else()
    # Assume x86/x64
    find_program(NASM_EXE nasm)

    if (NASM_EXE)
        set(BITS 64)
        set(ARCH x64)
        if (CMAKE_SIZEOF_VOID_P EQUAL 4)
            set(BITS 32)
            set(ARCH x86)
        endif()

        if (WIN32)
            add_definitions("-DWIN32 -D_WIN32 -DUNICODE -D_UNICODE")
            set(NASM_FORMAT win${BITS})
            set(SYS_NAME win32)
        else()
            set(NASM_FORMAT elf${BITS})
            set(SYS_NAME linux)
        endif()

        string(TOUPPER -D${SYS_NAME} NASM_ARGS)

        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/forth-${ARCH}.obj
            COMMAND ${NASM_EXE} -f ${NASM_FORMAT} ${NASM_ARGS} -g forth-${ARCH}.asm -o ${CMAKE_CURRENT_BINARY_DIR}/forth-${ARCH}.obj
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/forth-${ARCH}.asm ${CMAKE_CURRENT_SOURCE_DIR}/sys-${SYS_NAME}-${ARCH}.asm
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            )

        add_executable(forth ${CMAKE_CURRENT_BINARY_DIR}/forth-${ARCH}.obj)

        set_target_properties(forth PROPERTIES LINKER_LANGUAGE C)
        if (MSVC)
            target_link_options(forth PRIVATE /nodefaultlib)
        else()
            target_link_options(forth PRIVATE -nostdlib -static -Wl,--build-id=none)
        endif()
    endif()
endif()

if (MSVC)
    set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT cforth)
endif()
