module pixel_print_studio
    use uuid_module
    implicit none
    
    ! variables constantes 
    integer, parameter :: MAXI  = 4
    integer, parameter :: MINI = 2


    !::::::::::::::: Estrcutura de mi Matriz dispersa :::::::
    type :: node_val
        logical :: exists = .false.
        character (len=10) :: value
    end type node_val

    type :: Node
        integer :: row,column
        character(len = 10):: color = "white"
        type(Node), pointer :: up=> null(), down=> null(), right => null(), left=>null()
    end type 

    type :: Matriz
        type(Node), pointer :: root => null()
        integer :: width = 0
        integer :: height = 0
        contains 
        procedure :: initialize_matriz
        procedure :: insert
        procedure :: insertRowHeader
        procedure :: insertColumHeader
        procedure :: insertInRow
        procedure :: insertInColumn 
        procedure :: searchRow
        procedure :: searchColumn
        procedure :: existNode
        procedure :: showMatrixSparse
        procedure :: printColumnHeaders
        procedure :: getValue
        procedure :: addLayer

        procedure :: create_dot
        procedure :: get_content
        procedure :: aling_col_node
        procedure :: getNode
        procedure :: get_address_memory

        procedure :: create_table
        procedure :: get_table

        !:::::::: dot recorridos preorder, inorder, postorder
        procedure :: create_dot_recorridos
        procedure :: create_dot_image
        
    end type

    ! ::::::::::::::::::: Estructura BST ::::::::::::::::::::::::::::::::::::..
    type :: node_bst
        integer :: id_capa ! Guia para ordenar mi bst
        type(Matriz), pointer :: capa => null()
        type(node_bst), pointer :: left => null()
        type(node_bst), pointer :: right => null()
    end type

    type :: bst
        type(node_bst), pointer :: root => null()
        contains
        procedure :: add_bst
        procedure :: preorder  
        procedure :: inorder
        procedure :: postorder
        procedure :: amplitud 
        
        procedure :: per_layer
        procedure :: process_layer ! crear matriz combinada
        procedure :: text_preorden ! text label preorder
        procedure :: text_inorden  ! text label inorder
        procedure :: text_postorder ! text label postorder
        procedure :: search_node_bst ! buscar un node por medio de id y retornarlo
        procedure :: inicializate_bst ! inicializar el arbol
        procedure :: tree_bst_heigth ! obtener la altura del arbol
        procedure :: grap_bst ! graficar mi arbol b
        procedure :: process_level ! procesar por nivel
        procedure :: inorderTraversal

        ! listar por order (reportes usuario)
        procedure :: list_preoder
        procedure :: list_inorder
        procedure :: list_postorder
        procedure :: print_leaf_nodes

    end type

    ! :::::::::::::::::: Estructura AVL ::::::::::::::::::::::::::::::::::::::..

    type node_avl
        integer :: id_imagen
        integer :: height = 1
        type(bst), pointer :: tree_bst => null()
        type(node_avl), pointer :: left => null()
        type(node_avl), pointer :: right => null()
    end type

    type avl
        type(node_avl), pointer :: root => null()
        contains
        procedure :: add_avl 
        procedure :: preorder_avl
        procedure :: grap_avl
        procedure :: grap_avl_bst
        procedure :: delete_avl
        procedure :: search_node ! buscar nodo avl
    end type 

    ! :::::::::::::::: Estructura de mi Album :::::::::::::::::::::::::::::::
    type sub_list
        integer :: id_img
        type(sub_list), pointer :: next => null()
    end type

    type node_list
        character(len=36) :: album_name
        type(node_list), pointer :: next => null()
        type(sub_list), pointer :: list_i => null()
    end type

    type list_album
     type(node_list), pointer :: head => null()
     contains
        procedure :: add_album
        procedure :: add_list_img
        procedure :: remove_img_by_id
        procedure :: grap_album
    end type


    !:::::::::::::::::: estructura arbol B ::::::::::::::::::::::::::::::::::::
    type nodeptr
        type (node_treeB), pointer :: ptr => null()
    end type nodeptr

    type cliente
        integer*8 :: dpi !dpi
        character(len=20) :: nombre
        character(len=20) :: password
        type(bst) :: bst_tree
        type(avl) :: avl_tree
        type(Matriz) :: my_matriz
        type(list_album) :: my_album
        contains
            procedure :: report_info_admin  
            procedure :: report_infor_user
    end type

    type node_treeB  !Estrcutura principal Nodo arbol B
        type(cliente) :: cliente(0:5)
        integer :: num = 0 ! cuantas claves o valores
        type(nodeptr) :: link(0:5) !  link : son los que conectan un nodo con sus nodos hijos
    end type node_treeB

    type TreeB
        type(node_treeB), pointer :: root => null()
        contains 
            !subrutinas para insertar un cliente y modificarlo
            procedure :: insertB
            procedure :: setValue
            procedure :: insertNode
            procedure :: splitNode
            procedure :: createNode
            procedure :: traversal
            procedure :: create_dot_treeB
            procedure :: create_graph_treeB
            procedure :: search_node_Btree
            procedure :: modify_node_Btree

            ! subrutinas para eliminar un cliente
            procedure :: delete_client
            procedure :: delete_k
            procedure :: restablecer
            procedure :: combine
            procedure :: moveLeft
            procedure :: moveRigth
            procedure :: sucesor
            procedure :: quitar
            procedure :: deleteRegister
            procedure :: search_node_on_page
        
            ! subrutinas para verificar un cliente
            procedure :: exist_nodo
            procedure :: search_nodeB

            ! listar clientes por nivel (nombre, dpi, cantida de imagenes)
            procedure :: list_clients_by_level

    end type    

    !:::::: end estructura arbol B ::::::::


    !::::: Estrcutura de una lista simple para el recorrido por niveles
    type list_node
        type(nodeptr) :: data
        type(list_node), pointer :: next => null() 
    end type list_node


     !::::: Estructura de una lista simple para el top 5 imagenes con mayor numero de capas
    type list_node_layers
        integer :: id_imagen
        integer :: count_capas
        type(list_node_layers), pointer :: next => null()
    end type list_node_layers

    contains 

    ! ::::::::::: Funciones y subrutinas arbol B :::::::::::::::::::::::
    subroutine insertB(this, clienteP)
        class(TreeB), intent(inout) :: this  ! La instancia del árbol B sobre la que operamos.
        type(cliente), intent(in) :: clienteP  ! El valor a insertar.
        type(cliente) :: i 
        type(node_treeB), pointer :: child !nodo hijo
    
        allocate(child)
        if (this%setValue(clienteP, i, this%root, child)) then ! es verdadero
            this%root => this%createNode(i,child)
        end if  
    end subroutine insertB
    ! busca la ubicacion correcta para insertar el nodo y decide si es necesario partir la pagina
    recursive function setValue(this, val, pval, node_temp, child) result(res)
        class(TreeB), intent(inout) :: this
        type(cliente), intent(in) :: val
        type(cliente), intent(inout) :: pval
    
        type(node_treeB), pointer, intent(inout) :: node_temp
        type(node_treeB), pointer, intent(inout) :: child
        type(node_treeB), pointer :: newnode ! nuevo nodo
        integer :: pos
        logical :: res

        allocate(newnode)
        if (.not. associated(node_temp)) then
            pval = val
            child => null()
            res = .true. ! insertado un nodo
            return
        end if

        if (val%dpi < node_temp%cliente(1)%dpi) then ! nuevo valor < node_temp[1] ------- [   0 | root | ... | ... |  ... ]
            pos = 0
        else
            pos = node_temp%num
            do while (val%dpi < node_temp%cliente(pos)%dpi .and. pos > 1) ! si valor < ultimovalor &  valor > primervalor (se ingresara enmedio)    
                pos = pos - 1
            end do
            if (val%dpi == node_temp%cliente(pos)%dpi) then ! o el valor es igual a la posicon ultima
                print *, "No se permiten duplicados"
                res = .false.
                return
            end if
        end if

        if (setValue(this, val, pval , node_temp%link(pos)%ptr, child)) then
            if (node_temp%num < MAXI) then ! nodo actual < MAX nodos
                call this%insertNode(pval, pos, node_temp, child) ! insertar en el nodo y ordenarlo
            else
                call this%splitNode(pval, pval, pos, node_temp, child, newnode)
                child => newnode
                res = .true.
                return
            end if
        end if

        res = .false.
    end function setValue


    subroutine insertNode(this, val, pos, node_temp, child)
        class(TreeB), intent(inout) :: this
        type(cliente), intent(in) :: val
        integer, intent(in) :: pos
        type(node_treeB), pointer, intent(inout) :: node_temp
        type(node_treeB), pointer, intent(in) :: child
        integer :: j

        j = node_temp%num
        do while (j > pos)
            node_temp%cliente(j + 1) = node_temp%cliente(j) ! poner un posicion adelante el nodo 
            node_temp%link(j + 1)%ptr => node_temp%link(j)%ptr
            j = j - 1
        end do

        node_temp%cliente(j + 1) = val
        node_temp%link(j + 1)%ptr => child
        node_temp%num = node_temp%num + 1
    end subroutine insertNode


    subroutine splitNode(this, val, pval, pos, node_temp, child, newnode)
        class(TreeB), intent(inout) :: this
        integer, intent(in) :: pos
        type(cliente), intent(in) ::val
        type(cliente), intent(inout) :: pval
        type(node_treeB), pointer, intent(inout) :: node_temp, newnode
        type(node_treeB), pointer, intent(in) :: child
        integer :: median, i, j
        ! newnode valor hijo que vamos a insertar
        if (pos > MINI) then  
            median = MINI + 1  ! 2 + 1
        else
            median = MINI
        end if
    
        if (.not. associated(newnode)) then ! si no esta asociado newNode
            allocate(newnode) ! creamos newnode
            do i = 0, MAXI !
                newnode%link(i)%ptr => null()
            end do
        end if
    
        j = median + 1
        do while (j <= MAXI) 
            newnode%cliente(j - median) = node_temp%cliente(j) ! 15 10 15 ->20 "25"
            newnode%link(j - median)%ptr => node_temp%link(j)%ptr
            j = j + 1
        end do
        
        node_temp%num = median
        newnode%num = MAXI - median
    
        if (pos <= MINI) then
            call this%insertNode(val, pos, node_temp, child)
        else
            call this%insertNode(val, pos - median, newnode, child)
        end if
        !    left        padre       rigt 
        ! node_temp   hermano de   newnode
        pval = node_temp%cliente(node_temp%num) ! se aguarda en pval (valor padre) captura el valor que necesita ser promovido al nodo padre 
        newnode%link(0)%ptr => node_temp%link(node_temp%num)%ptr
        node_temp%num = node_temp%num - 1
    end subroutine splitNode
 
    function createNode(this, val, child) result(newNode)
        class(TreeB), intent(in) :: this
        type(cliente), intent(in) :: val ! valor a ingresar 
        type(node_treeB), pointer, intent(in) :: child
        type(node_treeB), pointer :: newNode
        integer :: i
        
        allocate(newNode)
        newNode%cliente(1) = val
        newNode%num = 1 ! cantidad de claves 
        newNode%link(0)%ptr => this%root ! arma al node hermano lef
        newNode%link(1)%ptr => child     ! arma al node hermano rigth
    
        do i = 2, MAXI
            newNode%link(i)%ptr => null()
        end do
    end function createNode

    recursive function search_node_Btree(this, myNode, dpi) result(foundNode)
        class(TreeB), intent(in) :: this
        type(node_treeB), pointer, intent(in) :: myNode
        type(node_treeB), pointer :: foundNode
        integer*8, intent(in) :: dpi
        integer :: i

        foundNode => null() ! establecemos nulo

        if(associated(myNode))then
            do i=0, myNode%num - 1
                if (myNode%cliente(i+1)%dpi == dpi)then
                    foundNode => myNode
                    print*, "dpi encontrado", myNode%cliente(i+1)%dpi
                    return
                end if
            end do

            do i=0 , myNode%num
                foundNode => this%search_node_Btree(myNode%link(i)%ptr, dpi)
                if(associated(foundNode))then
                    return
                end if
            end do
        end if
    end function search_node_Btree

    ! subrutina para verificar si existe un nodo por nombre y password
    recursive function exist_nodo(this, myNode, name, password, clientIndex) result(foundNode)
        class(TreeB), intent(in) :: this
        type(node_treeB), pointer, intent(in) :: myNode
        type(node_treeB), pointer :: foundNode
        character(len=*), intent(in) :: name, password
        integer, intent(out) :: clientIndex
        integer :: i    
        foundNode => null()
        clientIndex = 0
        if (associated(myNode))then
            do i = 0, myNode%num - 1
                if(myNode%cliente(i + 1)%nombre == trim(name) .and. &
                   myNode%cliente(i + 1)%password == trim(password))then
                   foundNode => myNode
                   clientIndex = i + 1
                   return 
                end if
            end do
            ! si no buscar en los nodos hijos
            do i = 0, myNode%num
                foundNode => this%exist_nodo(myNode%link(i)%ptr, name, password, clientIndex)
                if(associated(foundNode)) return 
            end do
        end if
    end function exist_nodo


    subroutine modify_node_Btree(this, dpi, new_name, new_p)
        class(TreeB), intent(in) :: this
        integer*8, intent(in) :: dpi
        integer :: i
        character(len=*), intent(in) :: new_name, new_p
        type(node_treeB), pointer :: found_node

        found_node => this%search_node_Btree(this%root, dpi)
        if(associated(found_node))then
            do i=0, found_node%num - 1
                if(found_node%cliente(i+1)%dpi == dpi)then
                    found_node%cliente(i+1)%nombre = new_name
                    found_node%cliente(i+1)%password = new_p
                    print *, "Cliente modificado exitosamente!"
                    return
                end if
            end do
        else 
            print *, "No existe el cliente con el dpi", dpi
        end if
    end subroutine


    recursive subroutine traversal(this, myNode)
        class(TreeB), intent(in) :: this
        type(node_treeB), pointer, intent(in) :: myNode
        integer :: i

        if (associated(myNode)) then
            write (*, '(A)', advance='no') ' [ '
            i = 0
            do while (i < myNode%num)
                write (*,'(A)', advance='no') myNode%cliente(i+1)%nombre
                i = i + 1
            end do
            do i = 0, myNode%num
                call this%traversal(myNode%link(i)%ptr)    
            end do
            write (*, '(A)', advance='no') ' ] '
        end if
    end subroutine traversal

    recursive subroutine create_dot_treeB(this, myNode, dotOutput, parentNodeName)
        class(TreeB), intent(in) :: this
        type(node_treeB), pointer, intent(in) :: myNode
        character(len=:), allocatable, intent(inout) :: dotOutput
        character(len=*), intent(in) :: parentNodeName
        character(len=256) :: nodeName
        integer :: i
   
        if (.not. allocated(dotOutput)) then
            dotOutput = "digraph G {" // new_line('a') 
            dotOutput = trim(dotOutput) // "rankdir=TB;" // new_line('a') 
            dotOutput = trim(dotOutput) // "nodesep=0.3;" // new_line('a') 
            dotOutput = trim(dotOutput) // "ranksep=1;" // new_line('a')
        end if

        if (associated(myNode)) then
            write(nodeName, '("Node_",I0)') loc(myNode)
            dotOutput = trim(dotOutput) //'"'// trim(adjustl(nodeName)) //'"  '// '[shape=record, label="' 
            
            do i = 0, myNode%num - 1
                dotOutput = trim(dotOutput) // myNode%cliente(i+1)%nombre
                if (i < myNode%num - 1) then
                    dotOutput = trim(dotOutput) // " | "
                end if
            end do

            dotOutput = trim(dotOutput) // '"];' // new_line('a')

            if (parentNodeName /= "") then
                dotOutput = trim(dotOutput) //'"'//trim(adjustl(parentNodeName))//'" '// " -> " // &
                '"'//trim(adjustl(nodeName))//'" '// ";" // new_line('a')
            end if

            do i = 0, myNode%num
                call this%create_dot_treeB(myNode%link(i)%ptr, dotOutput, nodeName)
            end do
        end if

        if (parentNodeName == "") then
            dotOutput = trim(dotOutput) // "}"  // new_line('a')  ! Fin del gráfico DOT
        end if
    end subroutine create_dot_treeB

    subroutine create_graph_treeB(this, filename)
        class(TreeB), intent(in) :: this
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: dot_out ! salida del dot 
        character(len=200) :: command
        integer :: unit

        call this%create_dot_treeB(this%root, dot_out, '')
        open(newunit = unit, file=filename//'.dot', status='replace', action='write')
        write(unit, '(*(A))') dot_out
        close(unit)

        call system('dot -Tpng ' // trim(filename) // '.dot -o ' // trim(filename) // '.png')
        command = 'start ' // trim(filename) // '.png'
        call system(command)

        print *, 'create image succefull!', trim(filename)//'.png'
    end subroutine create_graph_treeB 

    ! Crear funcion para tenorar .true. pos k  si existe si no .false.
    function search_node_on_page(this ,actual, value, k) result(repeated)
        class(TreeB), intent(inout) :: this
        type(node_treeB), pointer, intent(in) :: actual
        integer*8, intent(in) :: value
        integer, intent(out) :: k 
        logical :: repeated
        repeated = .false.

        if(associated(actual)) then 
            if(value < actual%cliente(1)%dpi) then
                k = 0
                repeated = .false.
            else
                k = actual%num
                do while(value < actual%cliente(k)%dpi .and. k > 1)
                    k = k - 1
                end do
                repeated = (value == actual%cliente(k)%dpi)
            end if
        end if
    end function search_node_on_page

    subroutine delete_client(this, valor)
        class(TreeB), intent(inout) :: this
        integer*8, intent(in) :: valor
        call this%delete_k(valor)
    end subroutine delete_client

    subroutine delete_k(this, value)
        class(TreeB), intent(inout) :: this
        integer*8, intent(in) :: value
        logical :: found 

        found = .false.
        call this%deleteRegister(this%root, value, found)

        if(found .and. associated(this%root))then
            if(this%root%num == 0)then
                if (associated(this%root%link(0)%ptr)) then
                    this%root => this%root%link(0)%ptr
                else
                    this%root => null()
                end if
            end if
        end if
    end subroutine delete_k

    ! delete register 
    recursive subroutine deleteRegister(this, actual, value, found)
        class(TreeB), intent(inout) :: this
        type(node_treeB), pointer, intent(inout) :: actual 
        integer*8, intent(in) :: value
        logical, intent(out) :: found
        integer :: k
        
        if(associated(actual))then
            found = this%search_node_on_page(actual, value, k)
            if (found) then
                if (.not. associated(actual%link(k-1)%ptr))then ! si eres un nojo hoja
                    call this%quitar(actual, k)
                else
                    call this%sucesor(actual, k)
                    call this%deleteRegister(actual%link(k)%ptr, actual%cliente(k)%dpi, found)
                end if
            else
                call this%deleteRegister(actual%link(k)%ptr, value, found)
            end if
            
            if(associated(actual%link(k)%ptr))then 
                if(actual%link(k)%ptr%num < MINI)then 
                    call this%restablecer(actual, k) 
                end if
            end if
        end if
    end subroutine deleteRegister

    ! funcion quitar 
    subroutine quitar(this, actual, k)
        class(TreeB), intent(inout) :: this
        type(node_treeB), pointer, intent(inout) :: actual
        integer, intent(in) :: k
        integer :: j
        do j = k + 1, actual%num
            actual%cliente(j-1) = actual%cliente(j)
            actual%link(j-1)%ptr => actual%link(j)%ptr 
        end do
        actual%num = actual%num - 1
    end subroutine quitar

    !funcion sucesor
    subroutine sucesor(this, actual, k)
        class(TreeB), intent(inout) :: this
        type(node_treeB), pointer, intent(inout) :: actual
        integer, intent(in) :: k
        type(node_treeB), pointer :: q
        q => actual%link(k)%ptr
        do while(associated(q%link(0)%ptr))
            q => q%link(0)%ptr
        end do
        actual%cliente(k) = q%cliente(1)
    end subroutine

    subroutine moveRigth(this ,actual, k)
        class(TreeB), intent(inout) :: this
        type(node_treeB), pointer, intent(inout) :: actual
        integer, intent(in) :: k
        type(node_treeB), pointer :: problemNode, leftNode
        integer :: j
        if(associated(actual%link(k)%ptr) .and. associated(actual%link(k-1)%ptr))then
            problemNode => actual%link(k)%ptr
            leftNode => actual%link(k - 1)%ptr ! donde voy a prestar

            do j = problemNode%num, 1, -1
                problemNode%cliente(j+1) = problemNode%cliente(j)
                problemNode%link(j+1)%ptr => problemNode%link(j)%ptr
            end do

            problemNode%num = problemNode%num + 1
            problemNode%link(1)%ptr => problemNode%link(0)%ptr

            problemNode%cliente(1) = actual%cliente(k)
            actual%cliente(k) = leftNode%cliente(leftNode%num)
            problemNode%link(0)%ptr => leftNode%link(leftNode%num)%ptr
            leftNode%num = leftNode%num - 1 
        end if
    end subroutine moveRigth

    subroutine moveLeft(this, actual, k)
        class(TreeB), intent(inout) :: this
        type(node_treeB), pointer, intent(inout) :: actual
        integer, intent(in) :: k
        integer :: i
        type(node_treeB), pointer :: problemNode, rightNode
        if(associated(actual%link(k)%ptr) .and. associated(actual%link(k-1)%ptr))then
            problemNode => actual%link(k - 1)%ptr
            rightNode => actual%link(k)%ptr

            if (associated(problemNode) .and. associated(rightNode))then

                problemNode%num = problemNode%num + 1
                problemNode%cliente(problemNode%num) = actual%cliente(k)
                problemNode%link(problemNode%num)%ptr => rightNode%link(0)%ptr
    
                actual%cliente(k) = rightNode%cliente(1)
                rightNode%link(1)%ptr => rightNode%link(0)%ptr
                rightNode%num = rightNode%num - 1
    
                do i = 1, rightNode%num
                    rightNode%cliente(i) = rightNode%cliente(i + 1)
                    rightNode%link(i)%ptr => rightNode%link(i + 1)%ptr
                end do
            end if
        end if
    end subroutine moveLeft

    subroutine combine(this ,actual, k)
        class(TreeB), intent(inout) :: this
        type(node_treeB), pointer , intent(inout) :: actual
        integer, intent(in) :: k
        integer :: j 
        type(node_treeB), pointer :: leftNode, q

        if(associated(actual%link(k)%ptr) .and. associated(actual%link(k-1)%ptr))then
            q => actual%link(k)%ptr 
            leftNode => actual%link(k - 1)%ptr 
            if(associated(leftNode) .and. associated(q))then
                leftNode%num = leftNode%num + 1
                leftNode%cliente(leftNode%num) = actual%cliente(k) 
                leftNode%link(leftNode%num)%ptr => q%link(0)%ptr

                do j = 1 , q%num 
                    leftNode%num = leftNode%num + 1
                    leftNode%cliente(leftNode%num) = q%cliente(j) 
                    leftNode%link(leftNode%num)%ptr => q%link(j)%ptr
                end do

                do j = k , actual%num - 1 
                    actual%cliente(j) = actual%cliente(j + 1)
                    actual%link(j)%ptr => actual%link(j + 1)%ptr
                end do
             actual%num = actual%num - 1
            end if
        end if
    end subroutine combine

    subroutine restablecer(this, actual, k)
        class(TreeB), intent(inout) :: this
        type(node_treeB), pointer ,intent(inout) :: actual
        integer, intent(in) :: k
        if (k > 0) then ! No es el primer hijo busca ayuda al hermano derecho k=0
            ! segunda entrada actual es 12 y 
            if (actual%link(k - 1)%ptr%num > MAXI / 2)then
                call this%moveRigth(actual, k)
            else
                call this%combine(actual, k)
            end if
        else 
            if (actual%link(1)%ptr%num > MAXI / 2)then
                call this%moveLeft(actual, 1)
            else
                call this%combine(actual, 1) ! 23,25 no es maximo
            end if
        end if
    end subroutine restablecer
   

    ! ::::::::::::::::::::::::::: end funciones y subruitnas arbol b :::::::::::::::::::::


    !::::::::::::::::::::::::::::: Subrutinas y funciones para Matriz Dispersa  :::::::::::::::::::::::::::::
    
    subroutine create_dot(self, filename)
        class(Matriz), intent(inout) :: self
        character(:), allocatable :: code
        character(len= *), intent(in) :: filename
        character(len=200) :: command
        integer :: unit

        code = "digraph G{" // new_line('a')
        code = code // ' node[shape=box];' // new_line('a')
        code = code // ' MTX[label = "Matrix", style= filled, fillcolor = firebrick1, group = 0];' // new_line('a')
        ! ejemplo para llamar mi contenido 
        code = code // self%get_content()
        code = code // '}' // new_line('a')
        open(newunit = unit , file= filename//'.dot', status='replace', action='write')
        write(unit, '(*(a))') code
        close(unit)
        print *, 'DOT code written to', filename // '.dot'

        call system('dot -Gnslimit=3 -Tpng ' // trim(filename) // '.dot -o ' // trim(filename) // '.png')
        command = 'start ' // trim(filename) // '.png'
        call system(command)

        print *, "Se grafico la matriz dispersa correctamente"
    end subroutine create_dot   

    subroutine create_dot_recorridos(self, txt_type_order, typeOrder, filename)
        class(Matriz), intent(inout) :: self
        character(:), allocatable :: code
        character(len=*), intent(in) :: txt_type_order
        character(len=*), intent(in) :: typeOrder
        character(len=*), intent(in) :: filename
        character(len=200) :: command
        integer :: unit

        code = "digraph G{" // new_line('a')
        code = code // ' node[shape=box];' // new_line('a')
        code = code // trim(typeOrder)//'[label ="'//trim(typeOrder)//':'//trim(txt_type_order)
        code = code //'"'//', style= filled, fillcolor = "#9DD5DF"];' // new_line('a')
        !code = code // ' MTX[label = "Matrix", style= filled, fillcolor = firebrick1, group = 0];' // new_line('a')
        ! ejemplo para llamar mi contenido 
        code = code // 'MTX [label=<' // new_line('a')
        code = code // self%get_table()
        code = code // '>]' // new_line('a')
        code = code // '}' // new_line('a')
        open(newunit=unit, file=trim(filename)//'.dot', status='replace', action='write')
        write(unit, '(*(a))') code
        close(unit)
        print *, 'DOT code written to', trim(filename) // '.dot'

        call system('dot -Tpng ' // trim(filename) // '.dot -o ' // trim(filename) // '.png')
        command = 'start ' // trim(filename) // '.png'
        call system(command)

        print *, 'PNG image generated at', trim(filename) // '.png'
    end subroutine create_dot_recorridos


    subroutine create_dot_image(self, filename)
        class(Matriz), intent(inout) :: self
        character(:), allocatable :: code
        character(len=*), intent(in) :: filename
        character(len=200) :: command  

        integer :: unit
        code = "digraph G{" // new_line('a')
        code = code // ' node[shape=box];' // new_line('a')
     
        ! ejemplo para llamar mi contenido 
        code = code // 'MTX [label=<' // new_line('a')
        code = code // self%get_table()
        code = code // '>]' // new_line('a')
        code = code // '}' // new_line('a')
        open(newunit=unit, file=trim(filename)//'.dot', status='replace', action='write')
        write(unit, '(*(a))') code
        close(unit)
        print *, 'DOT code written to', trim(filename) // '.dot'
        call system('dot -Tpng ' // trim(filename) // '.dot -o ' // trim(filename) // '.png')
        print *, 'PNG image generated at', trim(filename) // '.png'
        command = 'start ' // trim(filename) // '.png'
        call system(command)
    end subroutine create_dot_image

    function getNode(this, i, j) result(foundNode)
        class(Matriz), intent(in) :: this
        integer, intent(in) :: i, j
        type(Node), pointer :: rowHeader
        type(Node), pointer :: columnP
        type(Node), pointer :: foundNode
    
        foundNode => null()
        rowHeader => this%root%down  
        ! Recorre las cabeceras de fila
        do while(associated(rowHeader))
            if(rowHeader%row == i) then
                columnP => rowHeader%right  ! Inicia el recorrido por la fila i
                ! Recorre los nodos de la fila i
                do while(associated(columnP))  
                    if(columnP%column == j) then
                        foundNode => columnP  ! Asigna el nodo encontrado a foundNode
                        return  ! Sale de la función retornando foundNode
                    end if
                    columnP => columnP%right  ! Continúa a la derecha
                end do
                exit  ! Si encontramos la fila pero no la columna, salimos (el nodo no existe en esa fila)
            end if
            rowHeader => rowHeader%down  ! Continúa hacia abajo
        end do
        ! Si llegamos a este punto, no se encontró el nodo, foundNode ya es nulo
    end function getNode

    function get_address_memory(self, nodeCurrent) result(address)
        class(Matriz), intent(in) :: self
        type(Node), pointer :: nodeCurrent
        character(len=100) :: address
        integer*8 :: i

        i = loc(nodeCurrent)
        write(address, 10)i
        10 format(I0)
    end function get_address_memory
    
    function get_content(self) result(content)
        class(Matriz), intent(inout) :: self
        type(Node), pointer :: row_node
        type(Node), pointer :: pixel_temp
        character(:), allocatable :: createColNodes
        character(:), allocatable :: createRowNodes
        character(:), allocatable :: createMtxNodes
        character(:), allocatable :: alignCols
        character(:), allocatable :: alignRows
        character(:), allocatable :: linkNodes
        character(:), allocatable :: content
        

        ! variables enteras 
        integer :: i, j, aux_x, aux_y

        character(len=10) :: row_pos
        character(len=10) :: y_row_pos, str_aux_x, str_aux_y 
        character(len=100) :: address, address2
        character(len=10) ::  aux2, aux3

        createColNodes = ''
        createRowNodes = ''
        createMtxNodes = ''
        alignCols = ''
        linkNodes = ''
        content = ''

        aux_x = 0
        aux_y = 1

        call self%aling_col_node(createColNodes, alignCols, linkNodes)

        row_node => self%root%down
        write(row_pos, '(I0)') row_node%row
        linkNodes = linkNodes // '  "MTX" -> "f0' //'";'// new_line('a')
        
        do i=0, self%height
            write(y_row_pos, '(I0)') i
            createRowNodes = createRowNodes // ' "f'//trim(y_row_pos) // '"[label = "f'//trim(y_row_pos)
            createRowNodes = createRowNodes // '" style = filled, fillcolor = bisque1, group=0];' // new_line('a')
            alignRows = alignRows // '  { rank = same; ' // '"f'//trim(y_row_pos) // '";'
            do j=0 , self%width
                pixel_temp => self%getNode(i, j)
                if(associated(pixel_temp))then
                    address = self%get_address_memory(pixel_temp)
                    write(aux2, '(A)') pixel_temp%color
                    createMtxNodes = createMtxNodes // ' "'//trim(address)//'"[label="'//trim(aux2)
                    write(aux2, '(I0)') pixel_temp%column
                    createMtxNodes = createMtxNodes // '" group ='//trim(aux2)//', style = filled '
                    write(aux2, '(A)') pixel_temp%color
                    createMtxNodes = createMtxNodes // ', fillcolor = "'//trim(aux2)//'"];'//new_line('a')
                    ! enlazar nodos matriz UP, DOWN, LEFT, RIGTH
                    if(associated(pixel_temp%up))then
                        ! Link node UP
                        if(pixel_temp%up%row == -1) then
                            write(aux2, '(I0)') pixel_temp%column
                            linkNodes = linkNodes // ' "c'//trim(aux2)// '" -> "' // trim(address)// '";' // new_line('a')
                        else
                            address2 = self%get_address_memory(pixel_temp%up)
                            linkNodes = linkNodes // ' "'//trim(address)// '"->"' //trim(address2)  // '";'// new_line('a')
                        end if
                    end if
                    ! Link node LEF
                    if(associated(pixel_temp%left))then
                        if(pixel_temp%left%column == -1) then
                            write(aux3, '(I0)') pixel_temp%row
                            linkNodes = linkNodes // ' "f'//trim(aux3)// '" -> "' // trim(address) //'";'// new_line('a')
                        else
                            address2 = self%get_address_memory(pixel_temp%left)
                            linkNodes = linkNodes // ' "'//trim(address)//'"->"'//trim(address2)//'";'//new_line('a')
                        end if
                    end if
                    ! Link node DOWN
                    if(associated(pixel_temp%down))then
                        address2 = self%get_address_memory(pixel_temp%down)
                        linkNodes = linkNodes // ' "'//trim(address)//'"->"'//trim(address2)//'";'//new_line('a')
                    end if
                    ! Link node RIGHT
                    if(associated(pixel_temp%right))then
                        address2 = self%get_address_memory(pixel_temp%right)
                        linkNodes = linkNodes // ' "'//trim(address)//'"->"'//trim(address2)//'";'//new_line('a')
                    end if
                    alignRows = alignRows // '"' // trim(address) // '";'   
                end if
            end do
            alignRows = alignRows // '};' // new_line('a')
        end do
        


        do while(associated(row_node))
            do while(aux_x < row_node%row) 
                write(str_aux_x,'(I0)') aux_x
                write(str_aux_y, '(I0)') aux_y
                linkNodes = linkNodes // ' "f' // trim(str_aux_x)//'" -> "f'//trim(str_aux_y)//'";'// new_line('a')
                aux_x = aux_x + 1
                aux_y = aux_y + 1
            end do
            row_node => row_node%down
        end do

        content = createColNodes // createRowNodes // alignCols // alignRows // createMtxNodes // linkNodes
    end function get_content

    subroutine aling_col_node(self, createNodes, align, linkNodes)
        class(Matriz), intent(inout) :: self
        type(Node), pointer :: col_hdr_node
        character(:), allocatable :: linkNodes
        character(:), allocatable :: createNodes
        character(:), allocatable :: contenttemp
        character(:), allocatable :: align
        character(len=10) :: count
        character(len=10) :: aux
        character(len=10) :: aux2
        integer :: j, aux_j, z
        character(len=10) :: str_aux_j
        character(len=10) :: str_aux_z

        linkNodes = ''
        createNodes = ''
        contenttemp = ''
        align = ''
        
        z = 0
        aux_j = 1
        col_hdr_node => self%root%right
        linkNodes = linkNodes // ' "MTX" -> "c0'//'";'// new_line('a')
        align = '  { rank = same; "MTX";'
        do j = 0, self%width
            write(count, '(I0)') j
            createNodes = createNodes // ' "c'//trim(count) // '" [label = "c' // trim(count)
            createNodes = createNodes // ' "style= filled, fillcolor = bisque1, group = '//trim(count)//'];'// new_line('a')
            align = align // '"c' // trim(count)//'";'
        end do
        align = align // '};' // new_line('a')

        do while(associated(col_hdr_node))
            do while(z < col_hdr_node%column) 
                write(str_aux_z,'(I0)') z
                write(str_aux_j, '(I0)') aux_j
                linkNodes = linkNodes // ' "c' // trim(str_aux_z)//'" -> "c'//trim(str_aux_j)//'";'// new_line('a')
                z = z + 1
                aux_j = aux_j + 1
            end do
            col_hdr_node => col_hdr_node%right
        end do

        contenttemp = createNodes // align // linkNodes
    end subroutine aling_col_node

    
    function get_table(self) result(content)
        class(Matriz), intent(inout) :: self
        type(Node), pointer :: col_node
        character(:), allocatable :: content
        character(len=100) :: get_color
        integer :: i, j
    
        content = ''
        content = '<table border="0" cellborder="0" cellspacing="0" bgcolor="white">' // new_line('a')

        do i = 0, self%height
            content = content // '<tr>' // new_line('a')
            do j = 0, self%width
                col_node => self%getNode(i, j)

                if (associated(col_node)) then
                    get_color = 'bgcolor="' // trim(col_node%color) // '"'
                else
                    get_color = 'bgcolor="'//'#FFFFFF"'
                end if
                content = content // '<td ' // trim(adjustl(get_color))// '></td>'// new_line('a')
            end do
            content = content // '</tr>' // new_line('a')
        end do

        content = content // '</table>' // new_line('a')
    end function get_table

    subroutine create_table(self, filename)
        class(Matriz), intent(inout) :: self
        character(len=*), intent(in) :: filename
        character(:), allocatable :: code
        integer :: unit
        character(len=200) :: command 

        code = ''
        code = "digraph G {" // new_line('a')
        code = code // 'node [shape=box];' // new_line('a')
        code = code // 'MTX [label=<' // new_line('a')
    
        code = code // self%get_table()
    
        code = code // '>]' // new_line('a')
        code = code // "}"
    
        
        open(newunit=unit, file=trim(filename)//'.dot', status='replace', action='write')
        write(unit, '(*(a))') code
        close(unit)
    
        print *, 'DOT code written to', trim(filename) // '.dot'
    
        call system('dot -Tpng ' // trim(filename) // '.dot -o ' // trim(filename) // '.png')
        print *, 'PNG image generated at', trim(filename) // '.png'
        command = 'start ' // trim(filename) // '.png'  ! Para Windows
        call system(command)

    end subroutine create_table
    
    subroutine initialize_matriz(this)
        class(Matriz), intent(inout) :: this
        this%root => null()
        this%height = 0
        this%width = 0
    end subroutine

    subroutine insert(this, row ,column, color)
        class(Matriz), intent(inout) :: this
        integer, intent(in) :: row, column
        character (len=*), intent(in) :: color
        type(Node), pointer :: new, rowP, columnP
        allocate(new)
        new%row = row
        new%column = column
        new%color = color
 
        !Si no existe nodo en mi matrix entonces generamos la raiz (raiz= primer nodo)
        if(.not. associated(this%root))then
            allocate(this%root)
            this%root = Node(row=-1, column=-1, color="white")
        end if

        rowP => this%searchRow(row) 
        columnP => this%searchColumn(column)

        ! para saber el tamaño de mi matrix dispera necesitamos el width y height
        if(column > this%width) this%width = column
        if(row > this%height) this%height = row

        if(.not. this%existNode(new)) then
            ! Si no existe la cabezera columna entonces la crearemos
            if(.not. associated(columnP))then
                columnP => this%insertColumHeader(column)
            end if
            ! Su no existe la cabezera columna entonces la crearemos
            if(.not. associated(rowP))then
                rowP => this%insertRowHeader(row)
            end if

            call this%insertInColumn(new, rowP)
            call this%insertInRow(new, columnP)
        end if
    end subroutine insert

    function searchRow(this, row) result(current)
        class(Matriz), intent(inout) :: this
        integer, intent(in) :: row
        type(Node), pointer ::current
        current => this%root
        do while(associated(current)) 
            if(current%row == row) return
            current => current%down
        end do 
    end function searchRow

    function searchColumn(this, column) result(current)
        class(Matriz), intent(inout) :: this
        integer, intent(in) :: column
        type(Node), pointer :: current
        
        current => this%root
        do while(associated(current))
            if(current%column == column) return
            current => current%right ! Derecha ->
        end do
    end function searchColumn

    function existNode(this, new) result(isExist)
        class(Matriz), intent(inout) :: this
        type(Node), pointer :: new
        logical :: isExist

        type(Node), pointer :: rowHeader, columnCurrent
        rowHeader => this%root
        isExist = .false.
        do while(associated(rowHeader))
            if(rowHeader%row == new%row) then
                columnCurrent => rowHeader ! actual columna, donde esta fila actual
                do while(associated(columnCurrent))
                    if(columnCurrent%column == new%column) then
                        columnCurrent%color = new%color
                        isExist = .true.
                        return
                    end if
                    columnCurrent => columnCurrent%right
                end do
                return 
            end if
            rowHeader => rowHeader%down
        end do
    end function existNode

    function insertRowHeader(this, newRow) result(newRowHeader)
        class(Matriz), intent(inout) :: this
        integer, intent(in) :: newRow
        type(Node), pointer :: newRowHeader
        allocate(newRowHeader)
        newRowHeader = Node(row=newRow, column=-1)
        call this%insertInRow(newRowHeader, this%root) ! -1, newRow
    end function insertRowHeader

    subroutine insertInRow(this, new, columnHeader)
        class(Matriz), intent(inout) :: this
        type(Node), pointer :: new
        type(Node), pointer :: columnHeader
        type(Node), pointer :: current
        current => columnHeader
        do while(associated(current%down))
            if(new%row < current%down%row .and. new%row > current%row)then
                new%down => current%down
                new%up => current
                current%down%up => new
                current%down => new
                exit
            end if
            current => current%down
        end do

        if(.not. associated(current%down))then
            new%up => current
            current%down =>new
        end if
    end subroutine insertInRow

    function insertColumHeader(this, newColumn) result(newColumnHeader)
        class(Matriz), intent(inout) :: this
        integer, intent(in) :: newColumn
        type(Node), pointer :: newColumnHeader
        allocate(newColumnHeader)
        newColumnHeader = Node(row = -1, column = newColumn)
        call this%insertInColumn(newColumnHeader, this%root) ! -1, newColumnHeader
    end function

    subroutine insertInColumn(this, new, RowHeader)
        class(Matriz), intent(inout) :: this
        type(Node), pointer :: new
        type(Node), pointer :: RowHeader
        type(Node), pointer :: current
        current => RowHeader
        do while(associated(current%right))
            if(new%column < current%right%column .and. new%column > current%column)then
                new%right => current%right
                new%left => current
                current%right%left => new
                current%right => new
                exit
            end if
            current => current%right
        end do

        if(.not. associated(current%right))then
            current%right => new
            new%left => current
        end if
    end subroutine insertInColumn

    subroutine printColumnHeaders(this)
        class(Matriz), intent(in) :: this
        integer :: j
        character(len=20) :: headerStr

        do j=-1 , this%width
            write(headerStr, '(I5)') j
            write(*, fmt='(A6)', advance='no') trim(adjustl(headerStr)) // "  "
        end do
    end subroutine printColumnHeaders

    function getValue(this, i , j) result(val)
        class(Matriz), intent(in) :: this
        integer, intent(in) :: i, j
        type(Node), pointer :: rowHeader
        type(Node), pointer :: columnP
        type(node_val) :: val
        rowHeader => this%root

        do while(associated(rowHeader))
            if(rowHeader%row == i) then
                columnP => rowHeader
                do while(associated(columnP))
                    if(columnP%column == j) then
                        val%value = columnP%color
                        val%exists = .true.
                        return
                    end if
                    columnP => columnP%right
                end do
                return 
            end if
            rowHeader => rowHeader%down
        end do
    end function getValue
    
  
    subroutine showMatrixSparse(this)
        class(Matriz), intent(inout) :: this
        integer :: i, j
        type(node), pointer :: aux
        type(node_val) :: val
        aux => this%root%down
        call this%printColumnHeaders()

        do i = 0, this%height
            print *, ""
            write(*, fmt='(I3)', advance='no') i
            do j = 0, this%width
                val = this%getValue(i, j)
                if(.not. val%exists) then 
                    write(*, fmt='(A8)', advance='no') " #FFFFFF  "
                else
                    write(*, fmt='(A8)', advance='no') val%value
                end if
               
             end do
        end do
    end subroutine showMatrixSparse

    ! ::::::::::::::::::::::::::::::::::::::::::::::  Árbol BST ::::::::::::::::::::::::::::::::::::::::.
    ! consideraciones esenciales a tomar capa == matriz

    subroutine add_bst(this, id, capa)
        class(bst), intent(inout) :: this
        integer, intent(in) :: id
        type(Matriz), pointer, intent(in) :: capa
        type(node_bst), pointer :: temp

        allocate(temp)
        temp%id_capa = id
        temp%capa => capa
        
        if(associated(this%root))then
            call add_rec(this%root, temp)
        else
            this%root => temp
        end if
    end subroutine add_bst

    recursive subroutine add_rec(current, new)
        type(node_bst), pointer, intent(inout) :: current
        type(node_bst), pointer, intent(in) :: new

        if(new%id_capa < current%id_capa)then
            if(associated(current%left))then
                call add_rec(current%left, new)
            else 
                allocate(current%left)
                current%left => new
            end if

        else if(new%id_capa > current%id_capa)then
            if(associated(current%right))then
                call add_rec(current%right, new)
            else
                current%right => new
            end if
        end if
    end subroutine add_rec

    ! Inicializar mi arbol
    subroutine inicializate_bst(this)
        class(bst), intent(inout) :: this
        call delete_tree_branches(this%root)
        this%root => null() 
    end subroutine inicializate_bst

    ! Inicializar las ramas de mi arbol left, rigth
    recursive subroutine delete_tree_branches(node)
        type(node_bst), pointer :: node
        if (associated(node))then
            call delete_tree_branches(node%left)
            call delete_tree_branches(node%right)
            deallocate(node)
        end if
    end subroutine delete_tree_branches

    ! :::::: binary tree Preorder ::::::

    recursive subroutine preorder(this, current, matrizC , n , count)
        class(bst), intent(in) :: this
        type(node_bst), pointer, intent(in) :: current
        type(Matriz), pointer, intent(inout) :: matrizC
        integer, intent(in) :: n
        integer, intent(inout) :: count
        
        ! si el arbol bst no esta asociado left o right  return
        ! si count > n entonces a llegado hasta la cantida return
        if(.not. associated(current) .or. count > n) return 
        
        call this%process_layer(current%id_capa, matrizC) ! metodo para buscar y armar la matriz preorder
        
        count = count + 1
        
        if(count < n)then
            call this%preorder(current%left, matrizC ,n, count)
        end if

        if(count < n)then
            call this%preorder(current%right, matrizC ,n, count)
        end if

    end subroutine preorder 

    ! Obtener en chatareter mi preorder completo
    subroutine text_preorden(this, current, result) 
        class(bst), intent(inout) :: this
        class(node_bst), intent(in), pointer :: current
        character(len=*), intent(inout) :: result
        character(len=200) :: buffer
        
        if(.not. associated(current)) return
        
        write(buffer, '(I3)') current%id_capa
        result = trim(result)//' '//trim(buffer)

        call this%text_preorden(current%left, result)
        call this%text_preorden(current%right, result)
    end subroutine text_preorden
    
    ! ::::: binary tree Inorder ::::
    subroutine inorder(this, current, n, count, matriz_combined_inOrder)
        class(bst), intent(in) :: this
        type(node_bst),pointer , intent(in) :: current
        integer, intent(in) :: n
        integer, intent(inout) :: count
        type(Matriz), pointer, intent(inout) :: matriz_combined_inOrder  ! accediendo por valor de referencia

        if(.not. associated(current) .or. count > n) return

        if (count < n) then
            call this%inorder(current%left, n, count, matriz_combined_inOrder)
        end if
        if (count < n)then
            call this%process_layer(current%id_capa, matriz_combined_inOrder)
            count = count + 1
        end if
        if (count < n) then
            call this%inorder(current%right, n, count, matriz_combined_inOrder)
        end if
    end subroutine inorder

    ! Obtener en chatareter mi inorder completo
    subroutine text_inorden(this, current, result)
        class(bst), intent(in) :: this
        type(node_bst), pointer, intent(in) :: current
        character(len =*), intent(inout) :: result
        character(len=200) :: buffer

        if(.not. associated(current)) return
        call this%text_inorden(current%left, result)
        write(buffer, '(I3)') current%id_capa
        result = trim(result)//' '//trim(buffer)
        call this%text_inorden(current%right, result)
    end subroutine text_inorden

    ! ::::: binary tree Postorden ::::   
    subroutine postorder(this, current, n, count, matriz_combined_postOrder)
        class(bst), intent(in) :: this
        type(node_bst),pointer , intent(in) :: current
        integer, intent(in) :: n
        integer, intent(inout) :: count
        type(Matriz), pointer, intent(inout) :: matriz_combined_postOrder
                                        
        if(.not. associated(current) .or. count > n) return
        if(count < n)then
            call this%postorder(current%left, n, count, matriz_combined_postOrder)
        end if
        if(count < n)then
            call this%postorder(current%right, n, count, matriz_combined_postOrder)
        end if
        if(count < n)then
            call this%process_layer(current%id_capa, matriz_combined_postOrder)
            count = count + 1
        end if
    end subroutine postorder

    subroutine text_postorder(this, current, result)
        class(bst), intent(in) :: this
        type(node_bst), pointer , intent(in) :: current
        character(len=*), intent(inout) :: result
        character(len=200) :: buffer
        if(.not. associated(current)) return
        call this%text_postorder(current%left, result)
        call this%text_postorder(current%right, result)
        write(buffer, '(I3)') current%id_capa
        result = trim(result)//' '//trim(buffer)
    end subroutine text_postorder
    
    ! :::: binary tree amplitud
    ! ::::procesar todos los nodos por medio de cada nivel
    subroutine amplitud(this, matriz_combined)
        class(bst), intent(in) :: this
        type(matriz), pointer, intent(inout) :: matriz_combined
        integer :: h, level
        h = this%tree_bst_heigth(this%root)
        do level = 1, h
            call this%process_level(this%root, level, matriz_combined)  
        end do
    end subroutine amplitud

    !::::: recorrer por nivel 
    recursive subroutine process_level(this ,temp, level, matriz_com_ampl)
        class(bst), intent(in) :: this
        type(node_bst), pointer, intent(in) :: temp
        type(Matriz), pointer, intent(inout) :: matriz_com_ampl
        integer, intent(in) :: level
       
        if (.not. associated(temp)) return
        if (level == 1) then
            call this%process_layer(temp%id_capa, matriz_com_ampl)
        else
            call this%process_level(temp%left, level-1, matriz_com_ampl)
            call this%process_level(temp%right, level-1, matriz_com_ampl)
        end if
    end subroutine process_level

    ! ::::: find the max heigth 
    recursive function tree_bst_heigth(this, temp) result(h)
        class(bst), intent(in) :: this
        type(node_bst), pointer, intent(in) :: temp
            integer :: h_left, h_right, h

            if (.not. associated(temp)) then
                h = 0
                return 
            else
                h_left = this%tree_bst_heigth(temp%left)
                h_right = this%tree_bst_heigth(temp%right)
                h = 1 + max(h_left, h_right)
        end if
    end function tree_bst_heigth


    subroutine process_layer(this, id, matrizCombinada_preorder)
        class(bst), intent(in) :: this
        integer, intent(in) :: id
        type(Matriz), pointer :: currentMatriz
        type(Matriz), pointer ,intent(inout) :: matrizCombinada_preorder

        currentMatriz => search_bst(this%root, id)
        if(.not. associated(currentMatriz)) return
        
        if(.not. associated(matrizCombinada_preorder)) then
            allocate(matrizCombinada_preorder)
            call matrizCombinada_preorder%initialize_matriz()
        end if

        call matrizCombinada_preorder%addLayer(currentMatriz)
    end subroutine

    recursive subroutine inorderTraversal(this, current)
        class(bst), intent(in) :: this
        type(node_bst), pointer, intent(in) :: current

        if (.not. associated(current)) return
        call this%inorderTraversal(current%left)
        print *, 'Capa ID:', current%id_capa
        call showMatrixSparse(current%capa)
        call this%inorderTraversal(current%right)
    end subroutine inorderTraversal

    recursive function search_bst(current, search_id) result(capa)
        type(node_bst), pointer , intent(in) :: current
        integer, intent(in) :: search_id
        type(Matriz), pointer :: capa
        capa => null()

        if(.not. associated(current)) return ! Si no estas asociada a ninguna matriz retorna

        if(search_id < current%id_capa) then
            capa => search_bst(current%left, search_id)
        else if(search_id > current%id_capa) then
            capa => search_bst(current%right, search_id)
        else 
            capa => current%capa
        end if
    end function search_bst

    subroutine per_layer(this, id, matrizCombinada)
        class(bst), intent(in) :: this
        integer, intent(in) :: id
        type(Matriz), pointer, intent(inout) :: matrizCombinada
        type(Matriz), pointer :: currentMatriz


        currentMatriz => search_bst(this%root, id)
        if(.not. associated(currentMatriz)) then
            print *, "No fue encontrada mi matriz"
            return 
        end if

        if (.not. associated(matrizCombinada)) then
            allocate(matrizCombinada)
            call matrizCombinada%initialize_matriz()
        end if

        ! De lo contrario si ya esta asociada entonces 
        call matrizCombinada%addLayer(currentMatriz)
    end subroutine per_layer

    subroutine addLayer(this, nueva)
        class(Matriz), intent(inout) :: this
        class(Matriz), intent(in) :: nueva
        type(Node), pointer :: currentRow, currentNode
    
        ! Asumiendo que 'nueva' es otra instancia de Matriz
        
        currentRow => nueva%root%down
        do while(associated(currentRow))
            currentNode => currentRow%right
            do while(associated(currentNode))
                ! Usando 'insert' para añadir cada nodo a la instancia actual ('this').
                ! Nota: Asumimos que el método 'insert' ya maneja la lógica de no duplicar nodos.
                call this%insert(currentNode%row, currentNode%column, currentNode%color)
                currentNode => currentNode%right
            end do
            currentRow => currentRow%down
        end do
    end subroutine addLayer

    ! generar dot para el arbol bst 
    recursive subroutine printRec(root, name, unit)
        type(node_bst), pointer :: root
        character(len=36) :: name
        integer :: unit
        character(len=20) :: str_idcapa
        character(len=36) :: right
        character(len=36) :: left

        right = generate_uuid()
        left = generate_uuid()
        if(associated(root)) then
            write(str_idcapa, '(I0)') root%id_capa
       
            write(unit, *) '"Nodo'//name//'"[label = "'// trim(adjustl(str_idcapa))// '"]'
            if(associated(root%left))then
                write(unit, *)'"Nodo'//name//'"->"Nodo'//left//'"'
            end if
            if(associated(root%right))then
                write(unit, *)'"Nodo'//name//'"->"Nodo'//right//'"'
            end if
            call printRec(root%left, left, unit)
            call printRec(root%right, right, unit)

        end if
    end subroutine printRec

    subroutine grap_bst(this, filename)
        class(bst), intent(in) :: this
        character(len=*), intent(in) :: filename
        integer :: unit
        integer :: i
        character(len=100) :: dot_filename, png_filename, dot_command
        character(len=200) :: command

        dot_filename = trim(filename)//".dot"
        png_filename = trim(filename)//".png"
        dot_command = "dot -Tpng "//trim(dot_filename) // " -o "//trim(png_filename)

        unit = 1
        open(newunit=unit, file=trim(dot_filename))
        write(unit,*) "digraph G {"
        if(associated(this%root))then
            call printRec(this%root, generate_uuid(), unit)
        end if
        write(unit, *) "}"
        close(unit)

        call execute_command_line(trim(dot_command), exitstat=i)
        command = 'start ' // trim(filename) // '.png'
        call system(command)

        if(i /= 0)then
            print *, "Error al crear la grafica"
        else
            print *, "Se creo correctamente la grafica arbol b"
        end if
    end subroutine grap_bst

    
    ! :::::::::::::::::   Árbol AVL :::::::::::::::::::::::::::::::::::::::::

    subroutine add_avl(this, id, new_tree_bst)
        class(avl), intent(inout) :: this
        integer, intent(in) :: id
        type(bst), pointer, intent(in) :: new_tree_bst
        call add_avl_rec(this%root, id, new_tree_bst)
    end subroutine add_avl

    ! Insertar recursivamente
    recursive subroutine add_avl_rec(root, id, new_tree_bst)
        type(node_avl), pointer, intent(inout) :: root
        type(bst), pointer,  intent(in) :: new_tree_bst 
        integer, intent(in) :: id

        if (.not. associated(root)) then !si no estas asociado
            allocate(root)
            root%id_imagen = id
            root%tree_bst => new_tree_bst
            root%height = 1
        end if

        if (id < root%id_imagen) then
            call add_avl_rec(root%left, id, new_tree_bst)
        else if(id > root%id_imagen)then
            call add_avl_rec(root%right, id, new_tree_bst)
        end if

        root%height = maxheigth(getheight(root%left), getheight(root%right)) + 1

        ! Si es mayor a +1 entonces puede ser una rotacion (simple izquiera)
        if(get_balance(root) > 1) then
            if(get_balance(root%right) < 0)then ! doble derecha izquierda "-"
                root%right => right_rotation(root%right)
                root => left_rotation(root)
            else
                root => left_rotation(root) ! Aqui es una rotacion simple a la izquiera 
            end if
            
        end if

        if(get_balance(root) < -1)then
            if(get_balance(root%left) > 0)then 
                root%left => left_rotation(root%left)
                root => right_rotation(root)
            else
            root => right_rotation(root)
            end if
        end if
    end subroutine add_avl_rec

    ! Rotacion a la derecha
    function left_rotation(root) result(root_right)
        type(node_avl), pointer, intent(in) :: root
        type(node_avl), pointer :: root_right, temp

        root_right =>  root%right
        temp => root%right%left ! temporal si hubiera rigth%left
        root_right%left => root
        root%right => temp
        
        root%height = maxheigth(getHeight(root%left), getheight(root%right)) + 1 ! el root cambiamos su altura por que realizamos la rotacion
        root_right%height = maxheigth(getheight(root_right%left), getheight(root_right%right)) + 1 
    end function left_rotation

    ! Rotacion a la izquierda
    function right_rotation(root) result(root_left)
        type(node_avl), pointer , intent(in) :: root
        type(node_avl), pointer :: root_left, temp

        root_left => root%left ! apuntando a 4
        temp => root_left%right
        root_left%right => root
        root%left => temp

        root%height = maxheigth(getheight(root%left), getheight(root%right))+ 1
        root_left%height = maxheigth(getheight(root_left%left), getheight(root_left%right))+ 1
    end function right_rotation

    ! Obtenemos la altura de un nodo y retornamos
    function getheight(node) result(res_heigth)
        type(node_avl), pointer, intent(in) :: node
        integer :: res_heigth
        res_heigth = 0

        if(.not. associated(node)) return
        res_heigth = node%height ! obtenemos la altura del nodo
    end function getheight

    ! Comparamos la altura maxima
    function maxheigth(left, right) result(res)
        integer, intent(in) :: left
        integer, intent(in) :: right
        integer :: res
        res = right
        if(left >= right)then
            res = left
            return 
        end if
    end function maxheigth

    ! Obtener el balance
    function get_balance(root) result(balance)
        type(node_avl), intent(in) :: root
        integer :: balance
        balance = getheight(root%right) - getheight(root%left)
    end function get_balance

    recursive subroutine create_dot_avl_rec(root, name, unit)
        type(node_avl), pointer :: root
        character(len=36) :: name
        integer unit
        character(len=36) :: str_idImg
        character(len=36) :: right
        character(len=36) :: left

        left = generate_uuid()
        right = generate_uuid()

        if(associated(root)) then
            write(str_idImg, '(I0)') root%id_imagen
            write(unit, *)'"Nodo'//name//'"[label ="'//trim(adjustl(str_idImg))//'"]'

            if(associated(root%left))then
                write(unit, *)'"Nodo'//name//'"->"Nodo'//left//'"'
            end if

            if(associated(root%right))then
                write(unit, *)'"Nodo'//name//'"->"Nodo'//right//'"'
            end if

            call create_dot_avl_rec(root%left, left, unit)
            call create_dot_avl_rec(root%right, right, unit)
        end if
    end subroutine create_dot_avl_rec

    recursive subroutine create_dot_avl_bst_rec(root, name, unit, id)
        type(node_avl), pointer :: root
        character(len=36) :: name
        integer unit 
        integer, intent(in) :: id
        character(len=36) :: str_idImg
        character(len=36) :: right, left, root_bst_name
     
        left = generate_uuid()
        right = generate_uuid()
        root_bst_name = generate_uuid()

        if(associated(root))then
            write(str_idImg, '(I0)') root%id_imagen
            write(unit, *)'"Nodo'//name//'"[label ="'//trim(adjustl(str_idImg))//'", shape=box]'

            if(associated(root%left))then
                write(unit, *)'"Nodo'//name//'"->"Nodo'//left//'"'
            end if

            if(associated(root%right))then
                write(unit, *)'"Nodo'//name//'"->"Nodo'//right//'"'
            end if

            if(root%id_imagen == id .and. associated(root%tree_bst))then
                write(unit, *)'"Nodo'//name//'"->"Nodo'//root_bst_name//'"'
                call printRec(root%tree_bst%root, root_bst_name, unit)
            end if

            call create_dot_avl_bst_rec(root%left, left, unit, id)
            call create_dot_avl_bst_rec(root%right, right, unit, id)

        end if
    end subroutine create_dot_avl_bst_rec

    subroutine grap_avl_bst(this, filename, id_img)
        class(avl), intent(in) :: this
        character(len=*), intent(in) :: filename
        integer :: unit
        integer :: i
        character(len=100) :: dot_filename, png_filename, dot_command, command
        integer :: id_img
        unit = 1
        
        dot_filename = trim(filename)//".dot"
        png_filename = trim(filename)//".png"
        dot_command = "dot -Tpng " // trim(dot_filename) // " -o " // trim(png_filename)

        open(newunit=unit, file=dot_filename)
        
        write(unit, *) "digraph G {"
        if(associated(this%root)) then
            call create_dot_avl_bst_rec(this%root, generate_uuid(), unit, id_img)    
        end if
        write(unit, *) "}"
        close(unit)

        call execute_command_line(dot_command, exitstat=i)
        command = 'start ' // trim(filename) // '.png'
        call system(command)

        if (i == 1) then
            print *, "Error al crear la imagen"
        else 
            print *, "Se genero correctamente la imagen 3.3"
        end if
    end subroutine grap_avl_bst

    subroutine grap_avl(this, filename)
        class(avl), intent(in) :: this
        character(len=*), intent(in) :: filename
        integer :: unit
        integer :: i
        character(len=100) :: dot_filename, png_filename, dot_command
        character(len=200) :: command

        unit = 1
        
        dot_filename = trim(filename)//".dot"
        png_filename = trim(filename)//".png"
        dot_command = "dot -Tpng " // trim(dot_filename) // " -o " // trim(png_filename)

        open(newunit=unit, file=dot_filename)
        
        write(unit, *) "digraph G {"
        write(unit, *) "node [shape=box]"
        if(associated(this%root)) then
            call create_dot_avl_rec(this%root, generate_uuid(), unit)    
        end if
        write(unit, *) "}"
        close(unit)

        call execute_command_line(dot_command, exitstat=i)
        command = 'start ' // trim(filename) // '.png'
        call system(command)

        if (i == 1) then
            print *, "Error al crear la imagen"
        else 
            print *, "Se genero correctamente la imagen"
        end if
    end subroutine grap_avl

    recursive function deleteRec (currentNode, val) result(res)
        type(node_avl), pointer :: currentNode
        integer, intent(in) :: val
        type(node_avl), pointer :: res
        type(node_avl), pointer :: temp

        if(.not. associated(currentNode)) then
            res => currentNode 
            return 
        end if

        if(val < currentNode%id_imagen)then
            currentNode%left => deleteRec(currentNode%left, val)

        else if(val > currentNode%id_imagen)then
            currentNode%right => deleteRec(currentNode%right, val)
  
        else  ! Si no es mayor ni menor encontro el nodo a eliminar
            if(.not. associated(currentNode%left)) then
                temp => currentNode%right
                currentNode%tree_bst => null()
                deallocate(currentNode)
                res => temp
            else if(.not. associated(currentNode%right))then
                temp => currentNode%left
                currentNode%tree_bst => null()
                deallocate(currentNode)
                res => temp
            else  ! si hay un nodo izquierdo o derecho
                call get_mayor_of_minors(currentNode%left, temp) 
                currentNode%id_imagen = temp%id_imagen
                currentNode%tree_bst => temp%tree_bst ! ----
                currentNode%left => deleteRec(currentNode%left, temp%id_imagen)
            end if
        end if

        if(.not. associated(currentNode)) return

        currentNode%height = maxheigth(getheight(currentNode%left), getheight(currentNode%right)) + 1

        if(get_balance(currentNode) > 1)then
            if(get_balance(currentNode%right) < 0)then
                currentNode%right => right_rotation(currentNode%right)
                currentNode => left_rotation(currentNode)
            else
                currentNode => left_rotation(currentNode)
            end if
        end if

        if(get_balance(currentNode) < -1)then
            if(get_balance(currentNode%left) > 0)then
                currentNode%left => left_rotation(currentNode%left)
                currentNode => right_rotation(currentNode)
            else
                currentNode => right_rotation(currentNode)
            end if
        end if

        res => currentNode

    end function deleteRec

    subroutine delete_avl(this, id_imagen)
        class(avl), intent(inout) :: this
        integer, intent(in) :: id_imagen
        this%root => deleteRec(this%root, id_imagen)
    end subroutine delete_avl

    ! obtener el Mayor de los menores 
    recursive subroutine get_mayor_of_minors(root, nodeMayor)
        type(node_avl), pointer :: root
        type(node_avl), pointer :: nodeMayor
        ! obtenemos el derecho ya que siempre este sera el mayor
        if(associated(root%right))then
            call get_mayor_of_minors(root%right, nodeMayor)
        else
            nodeMayor => root
        end if
    end subroutine get_mayor_of_minors

    ! funcion para obtener el nodo del arbol avl si existe
    recursive function search_node_avl(nodeAvl, id_imagen) result(res)
        type(node_avl), pointer :: nodeAvl
        type(node_avl), pointer :: res
        integer, intent(in) :: id_imagen
        
        if(.not. associated(nodeAvl))then
            res => null()
            return
        end if

        if(id_imagen < nodeAvl%id_imagen)then
            res => search_node_avl(nodeAvl%left, id_imagen) 
        else if(id_imagen > nodeAvl%id_imagen)then
            res => search_node_avl(nodeAvl%right, id_imagen)
        else
            res => nodeAvl
        end if
    end function search_node_avl


    function search_node(this, id) result(found_node)
        class(avl), intent(in) :: this
        integer, intent(in) :: id
        type(node_avl), pointer :: found_node
        found_node => null()
        if(associated(this%root))then
            found_node => search_node_avl(this%root, id) 
        end if
    end function search_node



    recursive subroutine preorderRec(root)
        type(node_avl), pointer, intent(in) :: root

        if(associated(root)) then
            print *, root%id_imagen
            call preorderRec(root%left)
            call preorderRec(root%right)
        end if
    end subroutine preorderRec

    subroutine preorder_avl(this)
        class(avl), intent(in) :: this
        call preorderRec(this%root)
    end subroutine preorder_avl


    

  ! ::::::::: busqueda de un node_bst  para buscar mi nodo y retornalo  ::::::::: 
    function search_node_bst(this, id) result(found_node)
        class(bst), intent(in) :: this
        integer, intent(in) :: id
        type(node_bst), pointer :: found_node
        found_node => null()
        if(associated(this%root))then
            found_node => search_node_bst_rec(this%root, id)
        end if
    end function search_node_bst

    recursive function search_node_bst_rec(node, id) result(found_node)
        type(node_bst), pointer, intent(in) :: node
        integer, intent(in) :: id
        type(node_bst), pointer :: found_node
       
        found_node => null()

        if (associated(node)) then
            if (id == node%id_capa)then
                found_node => node
            else if(id < node%id_capa) then
                found_node => search_node_bst_rec(node%left, id)
            else 
            found_node => search_node_bst_rec(node%right, id)
            end if
        end if
    end function search_node_bst_rec


 !:::::::::::: Subrutina y funciones Album ::::::::::::::::::::::::::::

    subroutine add_album(this, name)
        class(list_album), intent(inout) :: this
        character(len=:), allocatable, intent(in) :: name
        type(node_list), pointer :: new_node, current

        allocate(new_node)
        new_node%album_name = name
        new_node%list_i  => null()
        new_node%next => null()

        if (.not. associated(this%head))then
            this%head => new_node
        else
            current => this%head
            do while(associated(current%next))
                current => current%next
            end do
            current%next => new_node
        end if
    end subroutine add_album

    subroutine add_list_img(this, album_name ,id_img)
        class(list_album), intent(inout) :: this
        type(node_list), pointer :: current_l
        type(sub_list), pointer :: new_subNode, current_sunNode
        integer, intent(in) :: id_img
        character(len=*), intent(in) :: album_name

        current_l => this%head
        do while(associated(current_l))
            if(trim(current_l%album_name) == trim(album_name)) then
                allocate(new_subNode)
                new_subNode%id_img = id_img
                new_subNode%next => null()

                if(.not. associated(current_l%list_i))then
                    current_l%list_i => new_subNode
                else    
                    current_sunNode => current_l%list_i
                    do while(associated(current_sunNode%next))
                        current_sunNode => current_sunNode%next
                    end do
                    current_sunNode%next => new_subNode
                end if
                return
            end if
            current_l => current_l%next
        end do
    end subroutine add_list_img


    ! remueve la imagen de la sublista si existe dentro de un álbum
    ! Si la imagen  del avl esta tambien en el album  tambien se actualiza
    subroutine remove_img_by_id(this, id_imagen)
        class(list_album), intent(inout) :: this
        integer , intent(in) :: id_imagen
        type(node_list), pointer :: current_album
        type(sub_list), pointer ::  current_img, prev_img
        
        current_album => this%head
        do while(associated(current_album))
            current_img => current_album%list_i
            prev_img => null()

            do while(associated(current_img))
                if(current_img%id_img == id_imagen)then
                    if(associated(prev_img))then
                        prev_img%next => current_img%next
                    else
                        current_album%list_i => current_img%next
                    end if
                    deallocate(current_img)
                    exit
                end if
                prev_img => current_img
                current_img => current_img%next
            end do

            current_album => current_album%next
        end do
        
    end subroutine remove_img_by_id

    ! grafica de lista de albunes 
    subroutine grap_album(this, filename)
        class(list_album), intent(in) :: this
        type(node_list), pointer :: current_album
        type(sub_list), pointer :: current_img, prev_img
        
        character(len=*), intent(in) :: filename
        integer :: albumCount, imageCount, unit
        character(len=200) :: albumNodeLabel, imageNodeLabel, prevImageNodeLabel, command
        character(len=50) :: albumCount_Str, imageCount_Str, idImage_Str, nextAlbumCount_Str
        
        albumCount = 0
        unit = 1
        open(unit, file=filename, status='replace')
        write(unit, *) "digraph G {"
        write(unit, *) "node [shape=box]"
        write(unit, *) 'rankdir=LR;'

        current_album => this%head
        do while(associated(current_album)) 
            write(albumCount_Str, '(I0)') albumCount
            albumNodeLabel = 'Album'//trim(albumCount_Str)
            write(unit, *) '"' // trim(albumNodeLabel) // &
            '"[label="'//trim(current_album%album_name) // '"]'

            current_img => current_album%list_i
            prev_img => null()
            imageCount = 0
            write(unit ,*) '{rank=same;'
            do while(associated(current_img))
                write(imageCount_Str, '(I0)') imageCount
                write(idImage_Str, '(I0)') current_img%id_img
                imageNodeLabel = 'Image'//trim(albumCount_Str)//'_'//trim(imageCount_Str)
                write(unit, *) '"' // trim(imageNodeLabel) // '" [label="Imagen: '//&
                trim(idImage_Str)//'"'//']'

                !link image
                if(imageCount == 0)then
                    write(unit, *) '"' // trim(albumNodeLabel) // '" -> "' // trim(imageNodeLabel) // '"'
                else
                    write(unit, *) '"' // trim(prevImageNodeLabel) // '" -> "' // trim(imageNodeLabel) // '"'
                end if
                
                prev_img => current_img
                prevImageNodeLabel = imageNodeLabel
                current_img => current_img%next
                imageCount = imageCount + 1

            end do
            write(unit, *) '}'

            if (associated(current_album%next)) then
                write(nextAlbumCount_Str, '(I0)') albumCount + 1
                write(unit, *) '"' // trim(albumNodeLabel) // '" -> "Album' // trim(nextAlbumCount_Str) // '"'
            end if

            albumCount = albumCount + 1
            current_album => current_album%next
        end do

        write(unit, *) '}'
        close(unit)

        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(filename) // '.png')
        command = 'start ' // trim(filename) // '.png'
        call system(command)
    end subroutine grap_album

    ! ::::::::: Reporte administrador ::::::::::::::::::::::::::::::::::::::::::::::::::::
    subroutine list_albums_and_images(this)
        class(list_album), intent(in) :: this
        type(node_list), pointer :: current_album
        type(sub_list), pointer :: current_image
        integer :: count_album

        count_album = 0
        current_album => this%head
        do while(associated(current_album))
            count_album = count_album + 1 
            print *, "Album:  ", trim(current_album%album_name)

            if(associated(current_album%list_i))then
                current_image => current_album%list_i
                do while(associated(current_image))
                    print*, "Imagen: ", current_image%id_img
                    current_image => current_image%next
                end do
            end if
            current_album => current_album%next
        end do
        print *, "Total de albunes: ", count_album
    end subroutine list_albums_and_images

    ! Obtener cantidad de imagenes del arbol avl
    recursive function get_count_image(currentNode) result(count_image)
        type(node_avl), pointer :: currentNode
        integer :: count_image

        if(.not. associated(currentNode))then
            count_image = 0
            return
        end if

        count_image = 1
        if(associated(currentNode%left))then
            count_image = count_image + get_count_image(currentNode%left)
        end if

        if(associated(currentNode%right))then
            count_image = count_image + get_count_image(currentNode%right)
        end if

    end function get_count_image

    ! Obtener cantidad de capas del arbol bst
    recursive function get_count_capas(currentNode) result(count_capas)
        type(node_bst), pointer :: currentNode
        integer :: count_capas

        if(.not. associated(currentNode))then
            count_capas = 0
            return
        end if

        count_capas = 1
        if(associated(currentNode%left))then
            count_capas = count_capas + get_count_capas(currentNode%left)
        end if

        if(associated(currentNode%right))then
            count_capas = count_capas + get_count_capas(currentNode%right)
        end if

    end function get_count_capas
    

    function total_nodes_avl(this) result(count_node)
        class(avl), intent(in) :: this
        integer :: count_node
        count_node = get_count_image(this%root)
    end function total_nodes_avl


    function total_nodes_bst(this) result(count_node)
        class(bst), intent(in) :: this
        integer :: count_node
        count_node = get_count_capas(this%root)
    end function total_nodes_bst
    
    ! Informacion de un Cliente
    subroutine report_info_admin(this)
        class(cliente), intent(in) :: this
        integer :: count_node_avl
        integer :: count_node_bst

        print*, ""
        print*, "DPI: ", this%dpi
        print*, "Nombre: ", trim(this%nombre)
        print*, "Password: ", trim(this%password)
        
        print *, ""
        print *, "Informacion del cliente"
        print *, "-----------------------"
        call list_albums_and_images(this%my_album) ! recorrer y obtener info de mis albunes

        count_node_avl = total_nodes_avl(this%avl_tree) ! obtener la cantidad de mis nodos avl
        count_node_bst = total_nodes_bst(this%bst_tree) ! obtener la cantidad de mis nodos bst

        print *, "Total de imagenes: ", count_node_avl
        print *, "Total de capas:",  count_node_bst
        print *, "-----------------------"
        print *, ""
    end subroutine report_info_admin


    ! encolar la lista list_module  
    subroutine enqueue(head, tail, item)
        type(list_node), pointer :: head
        type(list_node), pointer :: tail
        type(list_node), pointer :: item

        if(.not. associated(head))then
            head => item
            tail => item
        else
            tail%next => item
            tail => item
        end if
    end subroutine enqueue

    ! desencolar la lista list_module
    subroutine dequeue(head, tail, item)
        type(list_node), pointer :: head
        type(list_node), pointer :: tail
        type(list_node), pointer :: item

        if(associated(head))then
            item => head
            head => head%next
            if(.not. associated(head))then
                tail => null()
            end if
            nullify(item%next)
        end if
    end subroutine dequeue

    ! funcion que me retorna un valor bolleano si esta vacia mi lista
    function is_empty(head) result(empty)
        type(list_node), pointer :: head
        logical :: empty
        empty = .not. associated(head) ! .false.
    end function is_empty

    ! Listar clientes por nivel
    subroutine list_clients_by_level(this)
        class(TreeB), intent(in) :: this
        type(node_treeB), pointer :: current_node
        type(list_node), pointer :: head, tail, temp_node
        integer :: i, j, total_img
        integer :: nodes_current_level, nodes_next_level, level
        character(len=100) :: levelStr

        head => null()
        tail => null()
        level = 0

        if(associated(this%root))then
            allocate(temp_node)
            temp_node%data%ptr => this%root
            call enqueue(head, tail, temp_node)
            nodes_current_level = 1 ! nivel 1 la raiz
        end if

        do while(.not. is_empty(head))
            nodes_next_level = 0
            level = level + 1   
            write(levelStr, "('---------------- Nivel ', I0, ' ----------------')") level
            print *, trim(levelStr)

            do while(nodes_current_level > 0)
                call dequeue(head, tail, temp_node)
                current_node => temp_node%data%ptr
                deallocate(temp_node)
               
                !imprimir listado de clientes
                do i = 0, current_node%num - 1
                    total_img = total_nodes_avl(current_node%cliente(i + 1)%avl_tree)
                    print "(A, A, 3X, A, I20, 3X, A, I0)", "Nombre: ",&
                    trim(current_node%cliente(i + 1)%nombre), "DPI: ", current_node%cliente(i + 1)%dpi, &
                     "Cantidad de Imagenes totales: ", total_img

                end do

                do j = 0, current_node%num
                    if(associated(current_node%link(j)%ptr))then
                        allocate(temp_node)
                        temp_node%data%ptr => current_node%link(j)%ptr
                        call enqueue(head, tail, temp_node)
                        nodes_next_level = nodes_next_level + 1
                    end if
                end do
                nodes_current_level = nodes_current_level - 1
            end do
            nodes_current_level = nodes_next_level
        end do
    end subroutine list_clients_by_level

    ! Encontrar Nodo en un arbol por id ::: funcion para reporte administrador
    recursive function search_nodeB(this, myNode, dpi, clientIndex) result(foundNode)
        class(TreeB), intent(in) :: this
        type(node_treeB), pointer, intent(in) :: myNode
        type(node_treeB), pointer :: foundNode
        integer*8 :: dpi
        integer, intent(out) :: clientIndex
        integer :: i

        foundNode => null()
        clientIndex = 0

        if(associated(myNode))then
            do i = 0, myNode%num - 1
                if(myNode%cliente(i + 1)%dpi == dpi)then
                  foundNode => myNode
                  clientIndex = i + 1 
                  return
                end if
            end do

            do i = 0 , myNode%num
                foundNode => this%search_nodeB(myNode%link(i)%ptr, dpi, clientIndex)  
                if(associated(foundNode)) return              
            end do
        end if
    end function search_nodeB

    ! :::::::::::::::::::::::::::::::: Reporte Usuarios ::::::::::::::::::::::::::::::::::::::::::

    ! listar capas en preorder
    subroutine list_preoder(this, temp)
        class(bst), intent(in) :: this
        class(node_bst), intent(in), pointer :: temp
        if(.not. associated(temp)) return

        write(*, '(1I3)', advance='no') (temp%id_capa)
        
        call this%list_preoder(temp%left)
        call this%list_preoder(temp%right)
    end subroutine list_preoder

    ! listar capas en inorder
    subroutine list_inorder(this, temp)
        class(bst), intent(in) :: this
        class(node_bst), intent(in), pointer :: temp
        if(.not. associated(temp)) return

        call this%list_inorder(temp%left)
        write(*, '(1I3)', advance='no') (temp%id_capa)
        call this%list_inorder(temp%right)

    end subroutine list_inorder

    ! listar capas en postorder
    subroutine list_postorder(this, temp)
        class(bst), intent(in) :: this
        class(node_bst), intent(in), pointer :: temp
        if(.not. associated(temp)) return
        
        call this%list_postorder(temp%left)
        call this%list_postorder(temp%right)
        write(*, '(1I3)', advance='no') (temp%id_capa)

    end subroutine list_postorder

    ! Todas las capas que son hojas
    recursive subroutine print_leaf_nodes(this, temp)
        class(bst), intent(in) :: this
        class(node_bst), intent(in), pointer :: temp

        if(.not. associated(temp)) return

        if(.not. associated(temp%left) .and. .not. associated(temp%right))then
            write(*, '(1I3)', advance='no') (temp%id_capa)
        else
            if(associated(temp%left))then
                call this%print_leaf_nodes(temp%left)
            end if

            if(associated(temp%right))then
                call this%print_leaf_nodes(temp%right)
            end if
        end if
    end subroutine print_leaf_nodes

    ! Contruir nuestra lista nodos capas
    recursive subroutine build_image_list(currentNode, head)
        type(node_avl), pointer :: currentNode
        type(list_node_layers), pointer :: head, new_node, temp

        if(.not. associated(currentNode)) return
        allocate(new_node) ! asignar espacio a mi lista de nodos capas
        new_node%id_imagen = currentNode%id_imagen ! imagen del nodo avl actual
        new_node%count_capas = total_nodes_bst(currentNode%tree_bst)

        if(.not. associated(head))then
            head => new_node
        else
            new_node%next => head 
            head => new_node
        end if

        call build_image_list(currentNode%left, head)
        call build_image_list(currentNode%right, head)
    end subroutine build_image_list

    !subrutina para ordenar las 5 imagenes con mayor numero de capas
    subroutine bublle_sort(head)
        type(list_node_layers), pointer :: head, current_node, next_node
        integer :: tempId
        integer :: tempCount

        if(.not. associated(head)) then
            print *, "La lista esta vacia"
            return
        end if

        current_node => head
        do while(associated(current_node) .and. associated(current_node%next))
            next_node => current_node%next
            do while(associated(next_node))
                if(current_node%count_capas < next_node%count_capas) then
                    tempId = current_node%id_imagen
                    current_node%id_imagen = next_node%id_imagen
                    next_node%id_imagen = tempId

                    tempCount = current_node%count_capas
                    current_node%count_capas = next_node%count_capas
                    next_node%count_capas = tempCount
                end if
                next_node => next_node%next
            end do
            current_node => current_node%next
        end do
    end subroutine bublle_sort

    subroutine print_top_5_imagen(head)
        type(list_node_layers), pointer :: head
        type(list_node_layers), pointer :: current
        integer :: count

        current => head
        count = 0
        do while(associated(current) .and. count < 5)
            write(*, '(A, I5, A, I3)') "Imagen: ", current%id_imagen, " Numero capas: ", current%count_capas
            current => current%next
            count = count + 1
        end do
    end subroutine print_top_5_imagen

    subroutine report_top_images(this)
        class(avl), intent(in) :: this
        type(list_node_layers), pointer :: head, current, temp

        head => null()

        call build_image_list(this%root, head)

        call bublle_sort(head)

        print *, "Top 5 imagenes con mas capas: "
        call print_top_5_imagen(head)

        current => head
        do while(associated(current))
            temp => current%next
            deallocate(current)
            current => temp
        end do
    end subroutine report_top_images

    subroutine report_infor_user(this)
        class(cliente), intent(in) :: this
        integer :: altura_bst
        

        if(associated(this%avl_tree%root))then
            call report_top_images(this%avl_tree)
        end if

        if (associated(this%bst_tree%root))then 
            altura_bst = tree_bst_heigth(this%bst_tree, this%bst_tree%root)
            write(*, '(A, I0)') "Profundidad del arbol de capas: ", altura_bst
            print *, "Todas las capas que son hojas"
            call print_leaf_nodes(this%bst_tree, this%bst_tree%root)
            print *, ""
            print *, "--- Listar capas -- "
            print *, "Preorder"
            call list_preoder(this%bst_tree, this%bst_tree%root)
            print *, ""
            print *, "Inorder"
            call list_inorder(this%bst_tree, this%bst_tree%root)
            print *, ""
            print *, "PostOder"
            call list_postorder(this%bst_tree, this%bst_tree%root)
            print *, ""
        end if
    end subroutine report_infor_user

end module pixel_print_studio


program main
    use json_module
    use pixel_print_studio

    implicit none

    !variables contantes 
    character(len=*), parameter :: user_admin = "a"
    character(len=*), parameter :: password_admin = "a"

    ! variables para cargar archivos json 
    character(len=256) :: filename_load

    !variables para el menu principal
    integer :: op_menu_principal, op_menu_admin
    character(len=20) :: username_input
    character(len=20) :: password_input 

    !variables para la parte Administrador
    character(len=20) :: input_username, input_password ! variables , registrar usuario
    integer*8 :: input_dpi, input_dpi_report ! entra en consola para dpi, input_dpi_report = buscar dpi en el arbol b


    !variables para la parte Usuario
    integer :: op_menu_user, op_visualize_structure, op_gestion_image !opcion menu usuario
    integer :: selec_img ! id de la imagen seleccionada
    logical :: all_caps_exist ! bandera para ver si todas las capas estan en el arbol bst al momento de ingresar una imagen

    ! variables para el reporte de administrador
    integer :: op_report_admin


    ! variables para la parte Usuario - Generacion de imagenes por capa
    integer :: start_do, count_id, i
    integer, allocatable :: ids_capas(:)

    ! :::: TreeB  instance
    type(TreeB) :: my_treeB
    type(cliente) :: mycliente
    type(node_treeB), pointer :: clienteNode ! puntero para verificar si existe el usuario
    integer :: index, pos_c

    type(bst) :: myTree
    type(Matriz), pointer :: new_matriz => null() ! al momento de utilizar un puntero estamos obteniedo la referencia de memoria (nota: debemos de ver cuando implementos el cliente esta parte ya que la volverriamos a utilzar entonces se volver a inicializar)
    !::::: AVL instance
    type(bst), pointer :: newTree => null() ! al momento de utilizar un puntero estamos obteniedo la referencia de memoria (nota: debemos de ver cuando implementos el cliente esta parte ya que la volverriamos a utilzar entonces se volver a inicializar)
    type(avl) :: tree
    ! ::::: Variables para mi carga masiva de CLIENTES :::::::::::::.
    integer :: op_c1 
    


    ! test pruebas
    integer :: id_capa_img
    type(node_bst), pointer :: found_node => null()

    !preorder, inorder, postorder
    type(Matriz), pointer :: matrizC => null()
    integer :: numero_capas, count
    character(len=200) :: txt_label_order


    type(json_file) :: json
    type(json_core) :: jsonc

    ! PUNTEROS Y VARIABLES PARA LEER CLIENTES
    type(json_value), pointer :: p_list_C, p_cliente, p_atributos_C
    integer :: a, size_client
    logical :: found_client
    ! variables para asigar a mi cliente
    integer(kind=8) :: dpi_user
    character(:), allocatable :: dpi_str, name_user, password

    ! PUNTEROS Y VARIABLES PARA LEER CAPAS
    type(json_value), pointer :: p_list, p_capas, p_atributos
    type(json_value), pointer :: p_pixeles, p_pixel_atributos, p_fila, p_columna, p_color
    integer :: j, k, size, pixel_size
    logical :: found
    ! Variables ( Asignar a mi matriz y mi bst )
    integer :: id_capa, fila, columna
    
    ! PUNTEROS Y VARIABLES PARA LEER IMAGEN
    type(json_value), pointer :: p_list_I, p_imagenes, p_atributos_I
    integer :: w, x , size_img
    logical :: found_img
    integer :: capa_size
    type(json_value), pointer :: p_arr_capas, temp
    ! variables ( Asingar mi imagen avl)
    integer :: id_imagen, get_id_capa

    ! PUNTEROS Y VARIABLES PARA LEER ALBUM
    type(json_value), pointer :: p_list_A, p_albums, p_atributos_A, p_arr_imgs, temp_ptr
    integer :: p, q, size_album, size_arr_img
    logical :: found_album
    ! variables (Asingar mi album a lista de lista)
    character(len=:), allocatable :: name_album
    integer :: id_img_album
    

    
    !variables para -> opcion 4.1
    type(node_avl), pointer :: found_node_avl => null()
    integer :: op_generate_image, op_imagen_limit
    character(len=:), allocatable :: color_str
    
    call json%initialize() ! Se inicializa en json
   
   	! ::::  Menu Principal
    do while(op_menu_principal /= 4)
        print *, ":::::::::: Pixel Print Studio :::::::::"
        print *, "[1] Inicio de sesion admin"
        print *, "[2] Inicio de sesion user"
        print *, "[3] Registrarse"
        print *, "[4] Salir"
        read *, op_menu_principal
        select case (op_menu_principal)
            case(1)
                print *, " ----- Ingrese sus credenciales -----"
                print *, "Ingrese el nombre de usuario administrador:"
                read(*, '(A)') username_input
                print *, "Ingrese la contrasena:"
                read(*, '(A)') password_input
                if(trim(username_input) == user_admin .and. trim(password_input) == password_admin)then
                    print *, ""
                    print *, "Usuario adminisitrador"
                    op_menu_admin = 0
                    do while(op_menu_admin /= 7)
                        print *, "[1]  Carga Masiva de clientes"
                        print *, "[2]  Insertar Clientes"
                        print *, "[3]  Modificar Cliente"
                        print *, "[4]  Eliminar Cliente" 
                        print *, "[5]  Grafico de arbol B"
                        print *, "[6]  Reporte administrador"
                        print *, "[7]  Volver al menu incio"
                        print *, "Ingrese una opcion: "
                        read *, op_menu_admin
                        select case (op_menu_admin)
                            case (1) ! Carga masiva clientes 
                                call json%load(filename="clientes.json")
                                call json%info('', n_children=size_client)
                                call json%get_core(jsonc)
                                call json%get('', p_list_C, found_client)
                                do a = 1 , size_client
                                    dpi_str = ''
                                    name_user = ''
                                    password = ''  
                                    call jsonc%get_child(p_list_C, a, p_cliente, found = found_client)
                                    
                                    ! para dpi usuario
                                    call jsonc%get_child(p_cliente, 'dpi', p_atributos_C, found = found_client)
                                    if(found_client)then
                                        call jsonc%get(p_atributos_C, dpi_str)
                                        read(dpi_str, *) dpi_user !convertir string a int*8
                                    end if

                                    ! para nombre usuario
                                    call jsonc%get_child(p_cliente, 'nombre_cliente', p_atributos_C, found = found_client)
                                    if(found_client)then
                                        call jsonc%get(p_atributos_C, name_user)
                                    end if

                                    ! para password usuario
                                    call jsonc%get_child(p_cliente, 'password', p_atributos_C, found = found_client)
                                    if(found_client)then
                                        call jsonc%get(p_atributos_C, password)
                                    end if

                                    call my_treeB%insertB(cliente(dpi_user, name_user, password))  ! inserto en el arbol B

                                end do
                                print *, ''
                                print *, 'Carga masiva cargada exitosamente!'
                                print *, ''
                               
                            case (2) ! Insertar cliente
                                print *, "Ingrese el DPI del usuario:"
                                read(*, *) input_dpi
                                print *, "Ingrese el nombre del usuario:"
                                read(*, '(A)') input_username
                                print *, "Ingrese la contrasena del usuario:"
                                read(*, '(A)') input_password
                                call my_treeB%insertB(cliente(input_dpi, input_username, input_password))
                                print *, "Usuario creado con exito!"
                                call my_treeB%create_graph_treeB("grap_Btree_Insert")
                            case (3) ! Modificar cliente
                                print *, "Ingrese el DPI del usuario"
                                read(*, *) input_dpi
                                print *, "Ingrese el nuevo nombre del usuario"
                                read(*, '(A)') input_username
                                print *, "Ingrese la nueva contrasena del usuario"
                                read(*, '(A)') input_password
                                call my_treeB%modify_node_Btree(input_dpi, input_username, input_password)
                                print *, "Usuario modificado con exito!"
                                call my_treeB%create_graph_treeB("grap_Btree_Modidy")
                            case (4) ! Eliminar un cliente
                                Print *, "Ingrese el DPI del usuario a eliminar"
                                read(*, *) input_dpi
                                call my_treeB%delete_client(input_dpi)
                                call my_treeB%create_graph_treeB("grap_Btree_Delete")
                            case (5) ! Monstrar grafica del arbol B
                                call my_treeB%create_graph_treeB("grap_Btree")
                            case (6) ! reportes administrador
                                op_report_admin = 0 
                                do while(op_report_admin /= 3)
                                    print *, "[1] Buscar un cliente y mostrar su informacion"
                                    print *, "[2] Listar clientes por niveles"
                                    print *, "[3] Salir"
                                    print *, "Ingrese una opcion: "
                                    read *, op_report_admin
                                    select case(op_report_admin)
                                        case (1) !Buscar cliente
                                            print *, "Ingrese el DPI del cliente"
                                            read*, input_dpi_report
                                
                                            clienteNode => my_treeB%search_nodeB(my_treeB%root, input_dpi_report, pos_c)
                                            if(associated(clienteNode) .and. pos_c > 0)then
                                                call clienteNode%cliente(pos_c)%report_info_admin()
                                            end if

                                        case (2) !Listas clientes por niveles
                                            call my_treeB%list_clients_by_level()
                                    end select
                                end do
                                
                        end select
                    end do
                end if

            case(2)
                print *, " ----- Ingrese sus credenciales -----"
                print *, "Ingrese el nombre de usuario:"
                read(*, '(A)') username_input
                print *, "Ingrese la contrasena:"
                read(*, '(A)') password_input
                clienteNode => my_treeB%exist_nodo(my_treeB%root, username_input, password_input, index)
                    if(associated(clienteNode) .and. index > 0)then
                        op_menu_user = 0
                        do while(op_menu_user /= 6)
                            print *, " ::::: Menu Cliente ::::: "
                            print *, "[1] Carga masiva"
                            print *, "[2] Generacion de imagenes"
                            print *, "[3] Visualizar Estructuras"
                            print *, "[4] Gestion de imagenes"
                            print *, "[5] Reportes de usario"
                            print *, "[6] Volver al menu principal"
                            print *, "Ingrese una opcion"
                            read *, op_menu_user
                            select case(op_menu_user)
                                case(1) ! carga masiva 
                                op_c1 = 0
                                do while(op_c1 /= 4)
                                    print *, "::::::::: Carga Masiva de Cliente :::::"
                                    print *, "[1] carga masiva - capas"
                                    print *, "[2] carga masiva - imagenes"
                                    print *, "[3] carga masiva - album"
                                    print *, "[4] salir"
                                    print *, "Ingresar la opcion"
                                    read *, op_c1
                                    select case (op_c1)
                                        case (1) ! carga masiva - capas
                                            print *, "Ingrese el nombre del archivo JSON a cargar: "
                                            read(*, '(A)')filename_load
                                            call json%load(filename=trim(filename_load))
                            
                                            call json%info('', n_children=size)
                                            call json%get_core(jsonc)
                                            call json%get('', p_list, found)
                                            do j = 1 , size
                                                call jsonc%get_child(p_list, j, p_capas, found = found)
                                                
                                                id_capa = 0 !id_capa 
                                        
                                                call jsonc%get_child(p_capas, 'id_capa', p_atributos, found = found) 
                                                if (found) then
                                                    call jsonc%get(p_atributos, id_capa)    
                                                end if
                                        
                                                ! Acceder a la lista de pixeles
                                                call jsonc%get_child(p_capas, 'pixeles', p_pixeles, found= found)
                                                
                                                
                                                if (found) then
                                        
                                                    !le estoy dando un espacio en memoria a un puntero "new matriz"
                                                    allocate(new_matriz)
                                                    call new_matriz%initialize_matriz() ! inicializando datos
                                                    !call clienteNode%cliente(index)%my_matriz%initialize_matriz()
                                        
                                                    call jsonc%info(p_pixeles, n_children=pixel_size) ! tamaño de mi arreglo pixeles
                                        
                                                    do k = 1 , pixel_size    
                                        
                                                        fila = 0
                                                        columna = 0
                                                        color_str = ""
                                                        
                                                        call jsonc%get_child(p_pixeles, k, p_pixel_atributos, found = found)
                                                        
                                                        call jsonc%get_child(p_pixel_atributos, 'fila', p_fila, found = found)
                                                        if (found) then
                                                            call jsonc%get(p_fila, fila)
                                                        end if
                                        
                                                        call jsonc%get_child(p_pixel_atributos, 'columna', p_columna, found = found)
                                                        if (found) then
                                                            call jsonc%get(p_columna, columna)
                                                        end if
                                        
                                                        call jsonc%get_child(p_pixel_atributos, 'color', p_color, found = found)
                                                        if (found) then
                                                            call jsonc%get(p_color, color_str)
                                                        end if
                                                        
                                                        call new_matriz%insert(fila, columna, color_str)
                                                        !call clienteNode%cliente(index)%my_matriz%insert(fila, columna, color_str)
                                                    end do
                                                end if
                                                call clienteNode%cliente(index)%bst_tree%add_bst(id_capa, new_matriz)
                                            end do
                                            print *, "Se cargo las capas correctamente"  
                                            call json%destroy()   
                                        case (2) ! carga masiva - imagenes
                                            print *, "Ingrese el nombre del archivo JSON a cargar: "
                                            read(*, '(A)')filename_load
                                            call json%load(filename=trim(filename_load))
                                            call json%info('', n_children=size_img)
                                            call json%get_core(jsonc)
                                            call json%get('', p_list_I, found_img)
                                            do w = 1, size_img
                                                call jsonc%get_child(p_list_I, w, p_imagenes, found = found_img)
                                                id_imagen = 0 ! id_imagen
                                                call jsonc%get_child(p_imagenes, 'id', p_atributos_I, found = found_img)
                                                if (found_img)then
                                                    call jsonc%get(p_atributos_I, id_imagen)
                                                end if
                            
                                                allocate(newTree)
                                                call newTree%inicializate_bst() 
                            
                                                call jsonc%get_child(p_imagenes, 'capas', p_arr_capas, found_img)
                            
                                                if(found_img) then
                                                    call jsonc%info(p_arr_capas, n_children=capa_size)! numero de mi arreglo 
                                                    do x = 1 , capa_size
                                                    call jsonc%get_child(p_arr_capas, x, temp, found=found_img)
                                                    call jsonc%get(temp, get_id_capa)
                                                        
                                                        found_node => clienteNode%cliente(index)%bst_tree% &
                                                        search_node_bst(get_id_capa)
                                                        if(associated(found_node))then
                                                            call newTree%add_bst(found_node%id_capa, found_node%capa)
                                                        end if
                                                    end do
                                                end if
                                                ! aqui iria mi insert avl
                                                call clienteNode%cliente(index)%avl_tree%add_avl(id_imagen, newTree)
                                            end do
                                            print *, "carga masiva de imagenes correctamente"
                                            call json%destroy()
                                        case (3) ! carga masiva - album
                                            print *, "Ingrese el nombre del archivo JSON a cargar: "
                                            read(*, '(A)')filename_load
                                            call json%load(filename=filename_load)
                                            call json%info('', n_children=size_album)
                                            call json%get_core(jsonc)
                                            call json%get('', p_list_A, found_album)
                                            do p = 1 , size_album
                                                call jsonc%get_child(p_list_A, p, p_albums, found = found_album)
                                                call jsonc%get_child(p_albums, 'nombre_album', p_atributos_A, found = found_album)
                                                if(found_album)then
                                                    call jsonc%get(p_atributos_A, name_album)
                                                    call clienteNode%cliente(index)%my_album%add_album(name_album)
                                                end if
                                                
                                                call jsonc%get_child(p_albums, 'imgs', p_arr_imgs, found_album)
                                                if(found_album)then
                                                    call jsonc%info(p_arr_imgs, n_children=size_arr_img)
                                                    do q = 1, size_arr_img
                                                        call jsonc%get_child(p_arr_imgs, q, temp_ptr, found= found_album)
                                                        call jsonc%get(temp_ptr, id_img_album)
                                                        call clienteNode%cliente(index)%my_album%add_list_img(&
                                                        name_album, id_img_album)
                                                    end do
                                                end if 
                                            end do
                                            print *, "Se cargo el album de imagenes correctamente"     
                                            call json%destroy()
                                    end select
                                end do
                                case(2) ! generacion de imagenes
                                    ! declaracion de variables 4.1 Generacion de imagenes
                                    count = 0 
                                    txt_label_order = ''
                                    
                                    op_generate_image = 0
                                    op_imagen_limit = 0

                                    do while(op_generate_image /= 4)
                                        print *, ""
                                        print *, "-- GENERACION DE IMAGENES --"
                                        print *, "[1]  Recorrido Limitado"
                                        print *, "[2]  Por Arbol de Imagenes"
                                        print *, "[3]  Por capa"
                                        print *, "[4]  Salir"
                                        
                                        print *, "> Ingrese una opcion"
                                        read *, op_generate_image
                                        select case (op_generate_image)
                                        case (1) ! Generacion de imagenes por recorrido limitado
                                            do while(op_imagen_limit /= 4)
                                                print *, '[1] PreOrder'
                                                print *, '[2] InOrder'
                                                print *, '[3] PostOrder'
                                                print *, '[4] Volver a menu anterior'
                                                print *, "> Escoje un tipo de recorrido"
                                                read *, op_imagen_limit ! escojer una opcion para el tipo de recorrido
                                                select case (op_imagen_limit)
                                                    case (1) ! Recorrido en PreOrder
                                                        print *, "Ingrese el número de capas a utilizar"
                                                        read(*, *) numero_capas
                                                        if (associated(matrizC))then  ! si estas asociada , volver a utilizar el puntero
                                                            deallocate(matrizC)       ! entonces ya no quiero guardar mas esta matriz, libero espacio en memoria
                                                        end if
                                            
                                                        allocate(matrizC)              ! si no estas asociada asigno espacio de memoria
                                                        call matrizC%initialize_matriz() ! inicializo los valores
                                            
                                                        call clienteNode%cliente(index)%bst_tree%preorder(&
                                                        clienteNode%cliente(index)%bst_tree%root, matrizC, numero_capas, count)
                                                        
                                                        call clienteNode%cliente(index)%bst_tree%text_preorden(&
                                                        clienteNode%cliente(index)%bst_tree%root, txt_label_order)
                                              
                                                        if(associated(matrizC)) then
                                                            call matrizC%create_dot_recorridos(&
                                                            txt_label_order, "PREODER", "preoderImg")
                                                            count = 0 ! resetear count 
                                                            txt_label_order = '' ! resetear  txt_label
                                                        end if
                                                    case (2) ! Recorrido en InOrder
                                                        print *, "Ingrese el número de capas a utilizar"
                                                        read(*, *) numero_capas
                                                        if (associated(matrizC))then  
                                                            deallocate(matrizC)       
                                                        end if
                                                        
                                                        allocate(matrizC)             
                                                        call matrizC%initialize_matriz() 

                                                        call clienteNode%cliente(index)%bst_tree%inorder(&
                                                        clienteNode%cliente(index)%bst_tree%root, numero_capas, count, matrizC)
                                                        
                                                        call clienteNode%cliente(index)%bst_tree%text_inorden(&
                                                        clienteNode%cliente(index)%bst_tree%root, txt_label_order)

                                                        if(associated(matrizC))then
                                                            call matrizC%create_dot_recorridos(&
                                                            txt_label_order, "INORDER", "Inorder_Img")
                                                            count = 0
                                                            txt_label_order = ''
                                                        end if
                                                    case (3) ! Recorrido en PostOrder
                                                        print *, "Ingrese el número de capas a utilizar"
                                                        read(*, *) numero_capas
                                                        if(associated(matrizC))then
                                                            deallocate(matrizC)
                                                        end if

                                                        allocate(matrizC)
                                                        call matrizC%initialize_matriz()

                                                        call clienteNode%cliente(index)%bst_tree%postorder(&
                                                        clienteNode%cliente(index)%bst_tree%root, numero_capas, count, matrizC)
                                                        
                                                        call clienteNode%cliente(index)%bst_tree%text_postorder(&
                                                        clienteNode%cliente(index)%bst_tree%root, txt_label_order)

                                                        if(associated(matrizC))then
                                                            call matrizC%create_dot_recorridos(& 
                                                            txt_label_order, "POSTORDER", "PostOrder_Img")
                                                            count = 0
                                                            txt_label_order = ''
                                                        end if
                                                end select
                                            end do
                                        case (2) ! Generacion de imagenes por arbol de imagenes
                                            print *, "Ingrese el id de la imagen a buscar"
                                            read(*,*) selec_img
                                            found_node_avl => clienteNode%cliente(index)%avl_tree%search_node(selec_img)
                                            if(associated(found_node_avl))then
                                                if(associated(found_node_avl%tree_bst))then
                                                    
                                                    if(associated(matrizC))then
                                                        deallocate(matrizC)
                                                    end if
                                                    ! si no estas asociada
                                                    allocate(matrizC)
                                                    call matrizC%initialize_matriz()
                                                    call found_node_avl%tree_bst%amplitud(matrizC)
        
                                                    if(associated(matrizC))then
                                                        call matrizC%create_dot_image("Amplitud_Img")
                                                    end if
                                                end if  
                                            end if
                                        case (3) ! Generacion de imagenes por capa
                                            print *, "Ingrese el numero de id's de capas para generar la imagen"
                                            read(*, *) count_id

                                            if(allocated(ids_capas))then ! si tiene datos inicializar para volver a utilizar ids_capas
                                                deallocate(ids_capas)
                                            end if
                                            allocate(ids_capas(count_id))
                                            
                                            if(associated(matrizC))then
                                                deallocate(matrizC)
                                            end if

                                            ! si no estas asociado
                                            allocate(matrizC)
                                            call matrizC%initialize_matriz()
                                            
                                            ! Leer cada elemento
                                            print *, "Ingrese id's de las capas a graficar"
                                            do i = 1, count_id
                                                read(*, *) ids_capas(i)
                                                call clienteNode%cliente(index)%bst_tree%per_layer(ids_capas(i), matrizC)
                                            end do

                                            if(associated(matrizC))then
                                                call matrizC%create_table("imagen_por_capas.dot")
                                                deallocate(ids_capas)
                                            end if

                                        end select
                                    end do
                                case(3) ! vizualisacion de estructuras
                                    op_visualize_structure = 0
                                    do while(op_visualize_structure /= 6)
                                        print *, ":::::: Visualizacion del estado de las estructuras :::::"
                                        print *, "[1] Arbol Binario -> Capas"
                                        print *, "[2] Arbol Avl -> Imagenes "
                                        print *, "[3] Listado de Albuns"
                                        print *, "[4] Arbol Avl y ver sus capas"
                                        print *, "[5] Mostrar Capa - Matriz dispersa"
                                        print *, "[6] volver al menu usuario"
                                        print *, "Ingrese la estrcutura que desee vizualizar"
                                        read(*, *) op_visualize_structure
                                        select case(op_visualize_structure)
                                         case (1) 
                                            call clienteNode%cliente(index)%bst_tree%grap_bst("arbol_bst")
                                         case (2)
                                            call clienteNode%cliente(index)%avl_tree%grap_avl("arbol_avl")
                                         case (3)
                                            call clienteNode%cliente(index)%my_album%grap_album("list_album.dot")
                                         case (4)
                                            print *, "Ingrese el id de la imagen"
                                            read(*,*) selec_img
                                            call clienteNode%cliente(index)%avl_tree%grap_avl_bst("arbol_avl_bst", selec_img)
                                        case (5)
                                            print *, "Ingrese el id de la capa"
                                            read(*,*) selec_img
                                            
                                            if(associated(matrizC)) then ! Desalogar mi matriz para reutilizarla
                                                deallocate(matrizC)
                                            end if

                                            allocate(matrizC) ! Dar espacio de memoria a mi matriz
                                            call matrizC%initialize_matriz()
                                            call clienteNode%cliente(index)%bst_tree%per_layer(selec_img, matrizC)
                                            if(associated(matrizC))then
                                                call matrizC%create_dot("grap_matriz")
                                            end if
                                        end select 
                                    end do
                                case(4) ! Gestion de imagenes
                                    op_gestion_image = 0
                                    do while(op_gestion_image /= 3)
                                    print *, '[1] Registrar imagen'
                                    print *, '[2] Eliminar imagen'
                                    print *, '[3] Salir'
                                    print *, '> escoja una opcion'
                                    read*, op_gestion_image
                                    select case (op_gestion_image)
                                    case (1) ! Registrar imagen
                                        print *, "Ingrese el id de la imagen a crear"
                                        read(*,*) selec_img ! id_registrar
                                        found_node_avl => clienteNode%cliente(index)%avl_tree%search_node(selec_img)
                                        if(associated(found_node_avl))then
                                            print*,  "ya existe la imagen con el id", selec_img
                                        else
                                            print *, "Ingrese el numero de capas que obtendra la imagen"
                                            read(*, *) count_id
                                            
                                            if(allocated(ids_capas))then ! si esta asociado id's entonces desasignar memoria
                                                deallocate(ids_capas)
                                            end if
                                            
                                            allocate(ids_capas(count_id))
                                            
                                            allocate(newTree)  !agrego espacio en memoria a mi nuevo arbol bst
                                            call newTree%inicializate_bst()
                                            all_caps_exist = .true.

                                            print *, "Ingrese id's de las capas para la nueva imagen"
                                            do i = 1, count_id
                                                read(*, *) ids_capas(i)
                                                found_node => clienteNode%cliente(index)%bst_tree% &
                                                search_node_bst(ids_capas(i)) 
                                                if(associated(found_node)) then
                                                    call newTree%add_bst(found_node%id_capa, found_node%capa)
                                                else
                                                    print *, "No existe el id", ids_capas(i), "en el arbol de capas"
                                                    all_caps_exist = .false.
                                                    exit 
                                                end if
                                            end do
                                            
                                            if(all_caps_exist)then ! verficar si todas las capas pertenecen al arbol bst
                                                call clienteNode%cliente(index)%avl_tree%add_avl(selec_img, newTree)
                                                print *, "Nueva imagen agregada correctamente al arbol Avl"
                                                call clienteNode%cliente(index)%avl_tree%grap_avl("arbol_avl_ingresado")
                                            end if

                                        end if
                                    case (2) ! Eliminar imagen
                                        print *, "Ingrese el id de la imagen a eliminar"
                                        read(*,*) selec_img
                                        call clienteNode%cliente(index)%avl_tree%delete_avl(selec_img)
                                        print *, "Imagen eliminada con exito!"
                                        call clienteNode%cliente(index)%my_album%remove_img_by_id(selec_img)
                                        call clienteNode%cliente(index)%avl_tree%grap_avl("arbol_avl_eliminar")
                                                                                
                                    end select
                                    end do
                                case (5) ! Reportes de usuario
                                    print *, "Ingrese el DPI del cliente"
                                    read*, input_dpi_report
                                    clienteNode => my_treeB%search_nodeB(my_treeB%root, input_dpi_report, pos_c)
                                    if(associated(clienteNode) .and. pos_c > 0)then
                                        call clienteNode%cliente(pos_c)%report_infor_user()
                                    end if
                            end select
                        end do
                            
                       
                    else
                        print*, "No existe ese usuario con esas credenciales"
                    end if
                case(3) ! Registro de usuario
                    print *, "Registro de Usuario"
                    print *, "Ingrese su DPI"
                    read(*, *) input_dpi
                    clienteNode => my_treeB%search_nodeB(my_treeB%root, input_dpi, pos_c)
                     if(associated(clienteNode))then
                        print *, "Ya existe ese usuario por favor ingrese un DPI diferente"
                     else
                        print*, "Ingrese su nombre"
                        read(*, '(A)') input_username
                        print*, "Ingrese su contrasena"
                        read(*, '(A)') input_password
                        call my_treeB%insertB(cliente(input_dpi, input_username, input_password))
                        print *, "Usuario creado con exito!"
                        call my_treeB%create_graph_treeB("grap_Btree_Register_user")
                    end if
        end select
    end do
    
end program main