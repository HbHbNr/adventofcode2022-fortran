!> Solution for https://adventofcode.com/2022/day/19 part a
module day19a
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 162
    integer, parameter :: maxminutes = 24
    integer, parameter :: code_0 = iachar('0')
    integer, parameter :: code_9 = iachar('9')

    ! current recipe
    integer :: ore_bot_ore_costs
    integer :: clay_bot_ore_costs
    integer :: obsidian_bot_ore_costs
    integer :: obsidian_bot_clay_costs
    integer :: geode_bot_ore_costs
    integer :: geode_bot_obsidian_costs

    public :: solve

contains

    recursive subroutine rec_find_ql(minutesleft, max_geodes, &
                                     ores, clays, obsidians, geodes, &
                                     orebots, claybots, obsidianbots, geodebots)
        implicit none

        integer, intent(in)    :: minutesleft
        integer, intent(inout) :: max_geodes
        integer, intent(in)    :: ores
        integer, intent(in)    :: clays
        integer, intent(in)    :: obsidians
        integer, intent(in)    :: geodes
        integer, intent(in)    :: orebots
        integer, intent(in)    :: claybots
        integer, intent(in)    :: obsidianbots
        integer, intent(in)    :: geodebots
        logical, parameter     :: debug = .false.

        if (max_geodes < geodes) then
            print *, minutesleft, ': max geodes increased from ', max_geodes, ' to ', geodes
            max_geodes = geodes
        end if

        if (minutesleft == 0) return

        if (minutesleft > 1) then
            if (ores > geode_bot_ore_costs) then
                if (obsidians > geode_bot_obsidian_costs) then
                    if (debug) print *, minutesleft, ': build geode bot'
                    call rec_find_ql(minutesleft - 1, max_geodes, &
                                     ores + orebots - geode_bot_ore_costs, &
                                     clays + claybots, &
                                     obsidians + obsidianbots - geode_bot_obsidian_costs, &
                                     geodes + geodebots, &
                                     orebots, claybots, obsidianbots, geodebots + 1)
                end if
            end if
            if (ores > obsidian_bot_ore_costs) then
                if (clays > obsidian_bot_clay_costs) then
                    if (debug) print *, minutesleft, ': build obsidian bot'
                    call rec_find_ql(minutesleft - 1, max_geodes, &
                                     ores + orebots - obsidian_bot_ore_costs, &
                                     clays + claybots - obsidian_bot_clay_costs, &
                                     obsidians + obsidianbots, &
                                     geodes + geodebots, &
                                     orebots, claybots, obsidianbots + 1, geodebots)
                end if
            end if
            if (ores > clay_bot_ore_costs) then
                if (debug) print *, minutesleft, ': build clay bot'
                call rec_find_ql(minutesleft - 1, max_geodes, &
                                 ores + orebots - clay_bot_ore_costs, &
                                 clays + claybots, &
                                 obsidians + obsidianbots, &
                                 geodes + geodebots, &
                                 orebots, claybots + 1, obsidianbots, geodebots)
            end if
            if (ores > ore_bot_ore_costs) then
                if (debug) print *, minutesleft, ': build ore bot'
                call rec_find_ql(minutesleft - 1, max_geodes, &
                                 ores + orebots - ore_bot_ore_costs, &
                                 clays + claybots, &
                                 obsidians + obsidianbots, &
                                 geodes + geodebots, &
                                 orebots + 1, claybots, obsidianbots, geodebots)
            end if
        end if

        if (debug) print *, minutesleft, ': build nothing, just harvest'
        call rec_find_ql(minutesleft - 1, max_geodes, &
                         ores + orebots, &
                         clays + claybots, &
                         obsidians + obsidianbots, &
                         geodes + geodebots, &
                         orebots, claybots, obsidianbots, geodebots)
    end subroutine

    function find_quality_level(orgline) result(max_quality_level)
        implicit none

        character(len=*), intent(in)  :: orgline
        integer                       :: max_quality_level
        character(len=:), allocatable :: line
        integer                       :: i, max_geodes

        ! print *, '"', orgline, '"'

        line = orgline
        do i = 1, len(line)
            if (iachar(line(i:i)) >= code_0) then
                if (iachar(line(i:i)) <= code_9) then
                    cycle
                end if
            end if
            line(i:i) = ' '
        end do
        ! print *, '"', line, '"'
        ! read current recipe into global variables
        read (line, *) i, &
                       ore_bot_ore_costs, &
                       clay_bot_ore_costs, &
                       obsidian_bot_ore_costs, obsidian_bot_clay_costs, &
                       geode_bot_ore_costs, geode_bot_obsidian_costs
        ! print *, i, &
        !          ore_bot_ore_costs, &
        !          clay_bot_ore_costs, &
        !          obsidian_bot_ore_costs, obsidian_bot_clay_costs, &
        !          geode_bot_ore_costs, geode_bot_obsidian_costs

        max_geodes = 0
        call rec_find_ql(maxminutes, max_geodes, &
                         0, 0, 0, 0, &
                         1, 0, 0, 0)
        max_quality_level = max_geodes * i
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer                       :: i, total_quality_level, quality_level

        lines = readinputfile_asstringarray(filename, maxlinelength)

        total_quality_level = 0
        ! do i = 1, size(lines)
        !     ! if (i == 1) cycle
        !     quality_level = find_quality_level(lines(i))
        !     total_quality_level = total_quality_level + quality_level
        !     exit
        ! end do

        ! return maximum calories
        solve = total_quality_level
        solve = -1
    end function

end module day19a
