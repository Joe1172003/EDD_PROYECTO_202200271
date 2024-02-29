subroutine cliente_con_masPasos(this, filename)
    class(list_finalizados), intent(inout) :: this
    type(Nodo_finalizado), pointer :: current, cliente_con_mas_pasos
    integer :: fileUnit
    character(len=*), intent(in) :: filename

    if (.not. associated(this%head)) then
        print *, "La lista está vacía."
        return
    end if

    current => this%head
    cliente_con_mas_pasos => null()
    do while(associated(current))
        if (.not. associated(cliente_con_mas_pasos) .or. current%data_finalizado%total_pasos > cliente_con_mas_pasos%data_finalizado%total_pasos) then
            cliente_con_mas_pasos => current
        end if
        current => current%next
    end do

    if (.not. associated(cliente_con_mas_pasos)) then
        print *, "No se encontró un cliente válido."
        return
    end if

    ! Abrir archivo para escribir
    open(newunit=fileUnit, file=filename, status='replace')
    write(fileUnit, *) 'digraph ClienteConMasPasos {'
    write(fileUnit, *) 'rankdir=LR;'
    write(fileUnit, *) 'node [shape=box, style=filled, color="black", fillcolor="lightblue"]'

    ! Escribir nodo del cliente con más pasos
    write(fileUnit, *) '"Cliente" [label="Id Cliente: ', cliente_con_mas_pasos%data_finalizado%id_cliente, &
                        '|Nombre: ', trim(cliente_con_mas_pasos%data_finalizado%nombre_cliente), &
                        '|Total Pasos: ', cliente_con_mas_pasos%data_finalizado%total_pasos, '"];'

    write(fileUnit, *) "}"
    close(fileUnit)

    print *, "Graphviz file: ", trim(filename)
end subroutine cliente_con_masPasos
