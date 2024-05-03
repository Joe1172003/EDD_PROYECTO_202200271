module pixel_print_studio
    
    use iso_c_binding
    use json_module 

    implicit none

    !VARIABLES GLOBALES
    integer :: uid = 1
    integer :: index_dataBlock
    logical :: is_first_previusHash = .false. !BANDERA PARA DETERMINAR SI ES EL PRIMER DATA BLOCK
    character(len=:), allocatable :: previus_hash
    character(len=:), allocatable :: last_hash_dataBloke
    integer :: costos_totales = 0!costos totales para la empresa
    integer :: ingresos_extras_totales = 0
    integer :: ganancias_totales = 0 !ganancias totales para la empresa

    ! Hash table structure
    type techinical
        integer*8 :: dpi !key 
        character(len=32) :: name
        character(len=32) :: last_name
        character(len=20) :: genre
        character(len=100) :: direccion
        integer :: phone
        integer :: jobs_done = 0 ! trabajos realizados
    end type techinical

        
    type hash
        integer :: elements
        integer :: size_table
        integer :: maxi ! porcent of table hash
        type(techinical), dimension(:), allocatable :: table ! table techinical
        contains
            procedure :: init !inicializar tabla
            procedure :: division ! encontrar indice
            procedure :: linear_probe ! Colicion (doble dispersion)
            procedure :: insert !insertar en la tabla
            procedure :: show !mostrar tabla hash
            procedure :: reashing
            procedure :: find_technician !Buscar tecnico si existe 
            procedure :: graph_hash_table !Grafica de la tabla hash
            procedure :: find_technician_id !Buscar un tecnico es espesifico
            procedure :: list_all_technician !Listar tecnicos
    end type hash
    ! end Has table structure


    !:::: ESTRUCTURA LISTA SIMPLE PARA AGUARDAR TODOS LOS TECNICOS
    type node_techinical
        integer*8 :: dpi
        character(len=32) :: name
        character(len=32) :: last_name
        integer :: jobs_done = 0
        type(node_techinical), pointer :: next_techincal => null()
    end type node_techinical

    type all_techinical
        type(node_techinical), pointer :: head_techincal => nulL()
        contains
            procedure :: add_techical      ! AÑADIR TENCI A LA LISTA
            procedure :: find_one_techical ! BUSCAR UN TECNICO SI LO ENCUETRA SUMARLE 1 A JOBS DONE
            procedure :: bublle_sort_tech !ORDENAR POR NUMERO DE TRABAJOS REALIZADOS
            procedure :: top_5_tech !MOSTRAR 5 TECNICOS CON MAYOR NUMERO DE TRABAJOS REALIZADOS
            procedure :: show_all_technicians !MOSTRAR TODOS LOS TECNICOS
    end type all_techinical
    !:::::: END ESTRUCTURA PARA AGUARDAR TODOS LOS TECNICOS

    ! Sucursal Estructure
    type node_sucursal
        integer :: id_sucursal
        character(len=30) :: departament
        character(len=32) :: dirrecion
        character(len=32) :: password
        type(hash), allocatable :: tecnicos 
        type(node_sucursal), pointer :: left => null()
        type(node_sucursal), pointer :: right => null()
    end type node_sucursal

    type :: bst
        type(node_sucursal), pointer :: root => null()
        contains
            procedure :: add_bst
            procedure :: search_node_bst
            procedure :: inorderTraversal
            procedure :: search_node_bst_id !buscar una sucursal por id 
            procedure :: write_bst_to_json
    end type

    !Lista simple para capturar los valores del arbol bst
    type node_list_sucursal
        integer :: id_sucursal
        character(len=32) :: departament
        character(len=32) :: direccion
        integer :: request_jobs = 0
        type(node_list_sucursal), pointer :: next_suc => null()
    end type node_list_sucursal

    type list_sucursales
        type(node_list_sucursal), pointer :: head_list => null()
        contains
            procedure :: add_node_list_suc 
            procedure :: find_one_list_sucursal
            procedure :: bublle_sort_list_suc
            procedure :: top_5_sucusales
            procedure :: show_all_list_sucursales
    end type list_sucursales
    
    !::::::: Estrcutura del grafo ( lista de adyacencia )
    ! arista 
    type arista
        integer :: distance  ! distancia entre nodo Origen - nodo Vecino
        integer :: print_mant ! impresoar a mantenimiento
        type(arista), pointer ::  next_ari  ! siguiente arista 
        type(vertice), pointer :: dest_vertex ! vertice destino
        logical :: ari_visitada = .false. 
    end type arista

    !Vertice == nodo
    type vertice 
        integer :: vertex_id 
        type(vertice), pointer :: next_vertex
        type(arista), pointer :: ari
        
        integer :: acummulated_dis = 9999  ! acumular distancia
        integer :: printMant_dis = 0    ! acumular impresoras arregladas 
        logical :: visited = .false.
        type(vertice), pointer :: parent_nodo  ! para almacenar el la ruta padre -> hijo
    end type vertice

    ! Lista de vertices 
    type nodo_vertice
        type(vertice), pointer :: vertice
        type(nodo_vertice), pointer :: next
    end type nodo_vertice

    ! ::: Estructura para aguardar mis rutas 
    type ruta
        type(nodo_vertice), pointer :: vertice_head ! vertice cabeza  
        integer :: total_impresoras ! impresoras reparadas
        integer :: total_distancia ! total distancia
        type(ruta), pointer :: next_route ! siguiente ruta 
    end type ruta

    type grafo
        type(vertice), pointer :: first_vertex
        integer :: zise_graph   
        contains
            procedure :: initiGrafo
            procedure :: InsertArista
            procedure :: InsertaVertice
            procedure :: MostrarListaAdyacencia
            procedure :: GenerarGrafoDot
            procedure :: initialize_parameters !Inicializar parametros para reutilizar best_rooute
            procedure :: best_route ! Encontrar la distancia minima del grafo
            procedure :: best_route_maxPrintMant
            procedure :: find_all_routes
            procedure :: optimal_route
            procedure :: graph_best_route
    end type grafo

    ! Nodo cola
    type nodo_cola
        type(vertice), pointer :: vertex
        type(nodo_cola), pointer :: next_tail_vertx ! siguiente vertice 
    end type nodo_cola

    type cola_prioridad
        type(nodo_cola), pointer :: head => null()
            contains 
                procedure :: insert_priority_queue
                procedure :: extract_first_vertex
                procedure :: insert_maximum_queue
    end type

    !::: END Estructura Grafo

    !:::: Estructura Arbol Merkle
    type node_dataBloken ! data node
        integer :: uid
        character(len = :), allocatable :: value
        type(node_dataBloken), pointer :: next => null()
    end type node_dataBloken

    type hash_node !nodo hash
        integer :: uid
        character(len = :), allocatable :: hash
        type(hash_node), pointer :: left => null()
        type(hash_node), pointer :: right => null()
        type(node_dataBloken), pointer :: dataref => null()
    end type hash_node

    type tree_merkle
        type(hash_node), pointer :: tophash => null()
        type(node_dataBloken), pointer :: data_head => null()
        type(node_dataBloken), pointer :: data_coil => null()
        integer :: pos = 0
        contains
            procedure :: size_list ! tamaño de mi lista nodos hojas
            procedure :: add ! insertar nodos hoja
            procedure :: create_tree_merkle ! creo arbol merkle 
            procedure :: genhash !generar hash
            procedure :: datablok !obtengo los nodos hojas
            procedure :: showhash
            procedure :: show_dataBlock
            procedure :: generate
            procedure :: dot_tree_merkle
            procedure :: grap_Tree_merkle !graficar mi arbol merkle
            procedure :: get_root_hash
    end type tree_merkle
    !:::: end Estructura del arbol Merkle

    !:::: Estructuras BLOCK CHAIN
    type data 
        character(len=10) :: id_sucursal_o
        character(len=200) :: direccion_suc_o
        character(len=10) :: id_sucursal_d
        character(len=200) :: direccion_suc_d
        character(len=50) :: costo_suc 
        type(data), pointer :: next_data => null()
    end type data

    type all_data
        type(data), pointer :: head_data => null()
        contains
            procedure :: add_data
    end type

    type node_DataBlock
        integer :: index
        character(len=30) :: timestamp
        type(all_data) :: datablock
        character(len=20) :: nonce
        character(len=:), allocatable :: previus_hash
        character(len=:), allocatable :: root_merkle
        character(len=:), allocatable :: hash
        type(node_DataBlock), pointer :: next_DataBlock => null()
    end type node_DataBlock

    type block_chain
        type(node_DataBlock), pointer :: head_dataBlock => null()
        contains 
            procedure :: add_block !AÑADIR BLOCK CHAIN
            procedure :: show_block_chain
            procedure :: grap_block_chain !GRAFICAR BLOCK CHAIN
    end type block_chain

    contains

    !FUNCIONES Y SUBRUTINAS PARA EL ARBOL DE MERKLE

    !Añadir un nodo hoja al arbol Merkle
    subroutine add(this, value)
        class(tree_merkle), intent(inout) :: this
        character(len = :), allocatable, intent(in) :: value
        type(node_dataBloken), pointer :: temp
        allocate(temp)
        allocate(temp%value, source= value)
        temp%uid = uid
        uid = uid + 1
        if(associated(this%data_head))then 
            this%data_coil%next => temp !voy agregando temp
            this%data_coil => temp ! cola apunta a el temporal
        else
            this%data_head => temp
            this%data_coil => temp
        end if
    end subroutine add

    !Subrutina para obtener las potencias ^2,3,4,5,6
    subroutine generate(this)
        class(tree_merkle), intent(inout) :: this
        integer :: expo 
        integer :: i
        integer :: pow 
        character(len=:), allocatable :: valor
        valor = "-1"
        expo = 1 
        do while(2 ** expo < this%size_list()) !ver la potencia mas cercana a mi lista
            expo = expo + 1
        end do
        pow = 2 ** expo
        this%pos = pow
        i = this%size_list()
        do while(i < pow) ! iterar para que llene el arbol con nodos valor vacio
            call this%add(valor)
            i = i + 1
        end do
        allocate(this%tophash)
        call this%create_tree_merkle(this%tophash, expo) ! decendente para crear mi arbol
        call this%genhash(this%tophash, pow) ! aceedente para obtener mis hash
    end subroutine generate

    !Se crea el arbol asignando espacio de acuerdo al expo
    recursive subroutine create_tree_merkle(this, node, expo)
        class(tree_merkle), intent(inout) :: this
        type(hash_node), intent(inout), pointer :: node
        integer, intent(in) :: expo
        node%uid = uid
        uid = uid + 1
        if(expo > 0)then
            allocate(node%left)
            allocate(node%right)
            call this%create_tree_merkle(node%left, expo - 1)
            call this%create_tree_merkle(node%right, expo - 1)
        end if
    end subroutine create_tree_merkle

    !Se obtiene el genhash
    recursive subroutine genhash(this, node, pow)
        class(tree_merkle), intent(inout) :: this
        type(hash_node), intent(inout) ,pointer :: node
        type(node_dataBloken), pointer :: leaf_node

        integer, intent(in) :: pow
        integer :: temp
        character(len=:), allocatable :: hash
            if(associated(node))then
                call this%genhash(node%left, pow) !postorden
                call this%genhash(node%right, pow)
                if(.not. associated(node%left) .and. .not. associated(node%right))then
                    temp = pow - this%pos ! exp^3 => pow=8, this%pos = 8
                    node%dataref => this%datablok(temp)
                    this%pos = this%pos - 1
                    hash = node%dataref%value
                    node%hash = sha256(hash) !convertir a SHA256 la data
                else
                    hash = adjustl(node%left%hash(1:len(node%left%hash)/2))// &
                    adjustl(node%right%hash(1:len(node%right%hash)/2))
                    node%hash = sha256(hash) !convertir a SHA256 la data
                end if
            end if
    end subroutine genhash

    !Obtener el tamaño de mi lista (Nodos Hojas)
    function size_list(this) result(res)
        class(tree_merkle), intent(inout) :: this
        type(node_dataBloken), pointer :: temp
        integer :: res
        res = 0
        temp => this%data_head
        do while(associated(temp))
            res = res + 1 ! contador para saber cuantos nodos hay en mi lista
            temp => temp%next
        end do
    end function size_list 

    !obtiene el datablock 
    function datablok(this, pos) result(leaf_node) 
        class(tree_merkle), intent(inout) :: this
        integer, intent(inout) :: pos
        type(node_dataBloken), pointer :: leaf_node ! nodo hoja
        leaf_node => this%data_head
        do while(associated(leaf_node))
            if(pos == 0) return
            pos = pos - 1 
            leaf_node => leaf_node%next
        end do
    end function datablok

    subroutine grap_Tree_merkle(this)
        class(tree_merkle), intent(inout) :: this
        integer :: io, iostat, exitstat
        character(len=100) :: dot_command
        character(len=200) :: command

        open(newunit=io, file="merkle_tree.dot", status='replace', action='write', iostat=iostat)
        
        if (iostat /= 0) then
            print *, "Error al abrir el archivo grafo.dot"
            return
        end if

        write(io, '(A)') 'graph{'
        call this%dot_tree_merkle(this%tophash, io)
        write(io, '(A)') '}'
        
        close(io)
        
        dot_command = 'dot -Tpng merkle_tree.dot -o merkle_tree.png'
        call execute_command_line(dot_command, exitstat=exitstat)

        command = 'start '//'merkle_tree'//'.png'
        call system(command)

        if (exitstat == 0) then
            print *, "El árbol de Merkle fue generado exitosamente en 'merkle_tree.png'."
        else
            print *, "Error al generar el archivo PNG."
        end if

    end subroutine grap_Tree_merkle

    subroutine dot_tree_merkle(this, current, unit)
        class(tree_merkle), intent(inout) :: this
        type(hash_node), intent(in), pointer :: current
        integer, intent(in) :: unit 
        character(len=32) :: short_hash 

        if(.not. associated(current)) return
        short_hash = current%hash(1:32) ! Mostrar la mitad del hash 32 caracteres
        write(unit, '(A, I5, A, A, A)')' ', current%uid, '[label="', short_hash, '" shape=box];' !top hash

        if(associated(current%left))then
            write (unit, '(A,I5,A,I5,A)') ' ', current%uid, ' -- ', current%left%uid, ';'
        end if

        if(associated(current%right))then
            write (unit, '(A,I5,A,I5,A)') ' ', current%uid, ' -- ', current%right%uid, ';'
        end if

        call this%dot_tree_merkle(current%left, unit)
        call this%dot_tree_merkle(current%right, unit)

        if(associated(current%dataref))then
            write (unit, '(A, I5, A, A, A)') ' ', current%dataref%uid, ' [label="', current%dataref%value, '" shape=box];'
            write (unit, '(A, I5, A, I5, A)') ' ', current%uid, ' -- ', current%dataref%uid, ';'
        end if
    end subroutine dot_tree_merkle

    function get_root_hash(this) result (root_hash)
        class(tree_merkle), intent(in) :: this
        character(len=:), allocatable :: root_hash

        if(associated(this%tophash))then
            root_hash = this%tophash%hash
        else
            root_hash = "" !NO EXISTE ROOT HASH
        end if
    end function get_root_hash


    subroutine show_dataBlock(this)
        class(tree_merkle), intent(inout) :: this
        type(node_dataBloken), pointer :: temp
        temp => this%data_head
        do while(associated(temp))
            print *, temp%value
            temp => temp%next
        end do
    end subroutine show_dataBlock

    recursive subroutine showhash(this, temp)
        class(tree_merkle), intent(inout) :: this
        type(hash_node), pointer, intent(in) :: temp
        if(associated(temp))then
            write (*, '(A)', advance='no') " ( "
            write (*, '(A)', advance='no') temp%hash
            call this%showhash(temp%left)
            call this%showhash(temp%right)
            if(associated(temp%dataref))then
                write (*, '(A)', advance='no') "->"
                write (*, '(A)', advance='no') temp%dataref%value
            end if
            write (*, '(A)', advance='no') " ) "
        end if
    end subroutine showhash

    !:::: END FUNCIONES Y SUBRUTINAS PARA EL ARBOL DE MERKLE

    !:::: FUNCIONES Y SUBRUTINAS PARA BLOCK CHAIN

    !Subrutina insertar data en DATABLOCK
    subroutine add_data(this, id_suc_o, direccion_suc_o, id_suc_d, direccion_suc_d, costo_suc)
        class(all_data), intent(inout) :: this
        character(len=*), intent(in) :: id_suc_o, direccion_suc_o, id_suc_d, direccion_suc_d, costo_suc
        type(data), pointer :: new_data, current_data

        allocate(new_data)
        new_data%id_sucursal_o = id_suc_o
        new_data%direccion_suc_o = direccion_suc_o
        new_data%id_sucursal_d = id_suc_d
        new_data%direccion_suc_d = direccion_suc_d
        new_data%costo_suc = costo_suc
        new_data%next_data => null()

        if(.not. associated(this%head_data))then
            this%head_data => new_data
        else
            current_data => this%head_data
            do while(associated(current_data%next_data))
                current_data => current_data%next_data
            end do
            current_data%next_data => new_data
        end if
    end subroutine add_data


    !Subrutina insertar un bloque
    subroutine add_block(this, index, times_tamp, datablock, nonce, previus_hash, root_merkle, hash)
        class(block_chain), intent(inout) :: this
        integer, intent(in) :: index
        type(all_data), intent(in) :: datablock
        character(len=30), intent(in) :: times_tamp
        character(len=20), intent(in) :: nonce 
        character(len=:), intent(in) ,allocatable :: previus_hash, root_merkle, hash
        type(node_DataBlock), pointer :: current_block, new_block

        allocate(new_block)
        new_block%index = index
        new_block%timestamp = times_tamp
        new_block%datablock = datablock
        new_block%nonce = nonce
        new_block%previus_hash = previus_hash
        new_block%root_merkle = root_merkle
        new_block%hash = hash
        new_block%next_DataBlock => null()

        if(.not. associated(this%head_dataBlock))then
            this%head_dataBlock => new_block
        else
            current_block => this%head_dataBlock
            do while(associated(current_block%next_DataBlock))
                current_block => current_block%next_DataBlock
            end do
            current_block%next_DataBlock => new_block
        end if
    end subroutine add_block

    subroutine show_block_chain(this)
        class(block_chain), intent(in) :: this
        type(node_DataBlock), pointer :: current_block
        type(data), pointer :: current_data
    
        current_block => this%head_dataBlock
        if (.not. associated(current_block)) then
            print *, "The blockchain is empty."
            return
        end if
    
        do while(associated(current_block))
            print *, "Block Index:", current_block%index
            print *, "Timestamp:", trim(current_block%timestamp)
            print *, "Nonce:", trim(current_block%nonce)
            print *, "Previous Hash:", trim(current_block%previus_hash)
            print *, "Root Merkle:", trim(current_block%root_merkle)
            print *, "Hash:", trim(current_block%hash)
            print *, "Data in this block:"
    
            current_data => current_block%datablock%head_data
            if (.not. associated(current_data)) then
                print *, "  No data to display for this block."
            else
                do while(associated(current_data))
                    print *, "  - Sucursal Origen ID:", trim(current_data%id_sucursal_o)
                    print *, "  - Direccion Origen:", trim(current_data%direccion_suc_o)
                    print *, "  - Sucursal Destino ID:", trim(current_data%id_sucursal_d)
                    print *, "  - Direccion Destino:", trim(current_data%direccion_suc_d)
                    print *, "  - Costo:", trim(current_data%costo_suc)
                    current_data => current_data%next_data
                    if (associated(current_data)) print *, "  ----------------"
                end do
            end if
    
            current_block => current_block%next_DataBlock
            print *, "--------------------------------------------------------"
        end do
    end subroutine show_block_chain

    !Subrutina para mostrar la grafica del Block chain
    subroutine grap_block_chain(this)
        class(block_chain), intent(in) :: this
        type(node_DataBlock), pointer :: current_block
        type(data), pointer :: current_data
        character(len=100) :: dot_command
        integer :: unit, ios, exitstat
        character(len=200) :: command

        open(newunit=unit, file="block_chain.dot", status='replace', action='write', iostat=ios)
            
        if(ios /= 0) then
            print *, "Error al abrir el archivo"
            return
        end if

        write(unit, *) 'digraph G {'
        write(unit, *) 'node [shape=plaintext, fontname="Arial", fontsize=10];'
        
        current_block => this%head_dataBlock
        do while(associated(current_block))
            write(unit, '(A, I0, A)') '    block', current_block%index, ' [label=<<TABLE BORDER="1" CELLBORDER="0" CELLSPACING="0">'
            write(unit, *) '<TR><TD COLSPAN="2" BGCOLOR="lightgrey"><B>INDEX ', current_block%index, '</B></TD></TR>'
            write(unit, *) '<TR><TD>TIMESTAMP</TD><TD>', trim(current_block%timestamp), '</TD></TR>' 
            write(unit, *) '<TR><TD>NONCE</TD><TD>', trim(current_block%nonce), '</TD></TR>'
            write(unit, *) '<TR><TD>PREVIUS HASH</TD><TD>', trim(current_block%previus_hash), '</TD></TR>' 
            write(unit, *) '<TR><TD>ROOT MERKLE</TD><TD>', trim(current_block%root_merkle), '</TD></TR>' 
            write(unit, *) '<TR><TD>HASH</TD><TD>', trim(current_block%hash), '</TD></TR>'
            write(unit, *) '<TR><TD COLSPAN="2" BGCOLOR="lightblue">DATA</TD></TR>'

            current_data => current_block%datablock%head_data
            do while(associated(current_data))
                write(unit, *) '<TR><TD>ID SUCURSAL ORIGEN</TD><TD>', trim(current_data%id_sucursal_o), '</TD></TR>'
                write(unit, *) '<TR><TD>DIRECCION SUCURSAL ORIGEN</TD><TD>', trim(current_data%direccion_suc_o), '</TD></TR>'
                write(unit, *) '<TR><TD>ID SUCURSAL DESTINO</TD><TD>', trim(current_data%id_sucursal_d), '</TD></TR>'
                write(unit, *) '<TR><TD>DIRECCION SUCURSAL DESTINO</TD><TD>', trim(current_data%direccion_suc_d), '</TD></TR>'
                write(unit, *) '<TR><TD>COSTO</TD><TD>', trim(current_data%costo_suc), '</TD></TR>'
                current_data => current_data%next_data
                if (associated(current_data)) write(unit, *) '<TR><TD COLSPAN="2" BGCOLOR="silver"></TD></TR>'
            end do
        
            write(unit, *) '</TABLE>>];'

            if(associated(current_block%next_DataBlock)) then
                write(unit, '(A, I0, A, I0, A)') '    block', &
                current_block%index, ' -> block', current_block%next_DataBlock%index, ';'
            end if

            current_block => current_block%next_DataBlock
        end do

        write(unit, *) '}'
        close(unit)

        dot_command = 'dot -Tpng block_chain.dot -o block_chain.png'
        call execute_command_line(dot_command, exitstat=exitstat)

        command = 'start ' // 'block_chain' // '.png'
        call system(command)

        if (exitstat == 0) then
            print *, ":. La grafica del block_chain fue generado correctamente .:"
        else
            print *, "Error al generar el archivo PNG."
        end if

    end subroutine grap_block_chain
    
    !:::: END FUNCIONES Y SUBRUTINAS PARA BLOCK CHAIN


     ! Buscar una sucursal por id
    function search_node_bst_id(this, id) result(found_node)
        class(bst), intent(in) :: this
        integer, intent(in) :: id

        type(node_sucursal), pointer :: found_node
        found_node => null()
        if(associated(this%root))then
            found_node => search_node_bst_id_rec(this%root, id)
        end if
    end function search_node_bst_id

    ! buscar recursivamente en el arbol BST la sucursal por id
    recursive function search_node_bst_id_rec(node, id) result(found_node)
        type(node_sucursal), intent(in), pointer :: node
        integer, intent(in) :: id
        

        type(node_sucursal), pointer :: found_node

        found_node => null()
        if(associated(node))then
            if(node%id_sucursal == id)then
                found_node => node
            else if(id < node%id_sucursal)then
                found_node => search_node_bst_id_rec(node%left, id)
            else if(id > node%id_sucursal)then
                found_node => search_node_bst_id_rec(node%right, id)
            end if
        end if
    end function search_node_bst_id_rec 
    !::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    !Inicializar todas las aritas como ari_visitado = .false. 
    subroutine initialize_aristas(graph)
        type(grafo), intent(inout) :: graph
        type(vertice), pointer :: v
        type(arista), pointer :: a

        v => graph%first_vertex
        do while(associated(v))
            a => v%ari
                do while(associated(a))
                    a%ari_visitada = .false.
                    a => a%next_ari
                end do
                v => v%next_vertex
        end do
    end subroutine initialize_aristas

    !Añadir ruta
    subroutine addRoute(head ,new_route)
        type(ruta), pointer :: head, new_route, current

        if(.not. associated(head))then
            head => new_route
        else
            current => head
            do while(associated(current%next_route))
                current => current%next_route
            end do
            current%next_route => new_route
        end if
    end subroutine addRoute

    !  ::: Prodecimientos y subrutinas para insertar vertices y arista  grafica
    subroutine initArista(vertx, vertx_dest , new_distance, new_print_mant)
        type(arista), intent(inout) :: vertx
        type(vertice), pointer, intent(in) :: vertx_dest
        integer, intent(in) :: new_distance
        integer, intent(in) :: new_print_mant
        
        vertx%distance = new_distance   ! asisgar distancia
        vertx%print_mant = new_print_mant ! asignar impresoras a mantenimiento
        vertx%dest_vertex => vertx_dest ! Asignar vertice destino
        nullify(vertx%next_ari) ! inicializar siguiente arista
    end subroutine initArista
    
    ! Inicializar grafo
    subroutine initiGrafo(this)
        class(grafo), intent(inout) :: this
        nullify(this%first_vertex)
        this%zise_graph = 0
    end subroutine initiGrafo

    ! Funcion para ver si el grafo esta vacio
    function Graph_is_empty(this) result(is_empty)
        type(grafo), intent(in) :: this
        logical :: is_empty
        is_empty = (this%zise_graph == 0) ! si es 0 es true = esta vacio
    end function Graph_is_empty

    ! Funcion para obtener el tamaño del grafo
    function obtener_tamano(this) result(size)
        type(grafo), intent(in) :: this
        integer :: size
        size = this%zise_graph
    end function obtener_tamano

    !Obtener vertice 
    function ObtenerVertice(this, id_vertex) result(vertex_found)
        type(grafo), intent(in) :: this
        integer, intent(in) :: id_vertex
        type(vertice), pointer :: vertex_current ,vertex_found

        vertex_current => this%first_vertex
        vertex_found => null()

        do while(associated(vertex_current))
            if(vertex_current%vertex_id == id_vertex)then
                vertex_found => vertex_current
                exit
            end if 
            vertex_current => vertex_current%next_vertex
        end do
    end function ObtenerVertice

    !Buscar vertice Si existe retorna true, Si no retorna false
    function BuscarVertice(this, id_vertex) result(is_exist)
        class(grafo), intent(in) :: this
        integer, intent(in) :: id_vertex
        logical is_exist
        type(vertice), pointer :: vertex_current
        vertex_current => this%first_vertex
        is_exist = .false. !Inicializar como falso 

        do while(associated(vertex_current))
            if(vertex_current%vertex_id == id_vertex)then
                is_exist = .true.
                exit
            end if 
            vertex_current => vertex_current%next_vertex
        end do
    end function BuscarVertice

    !Insertar una vertice 
    subroutine InsertaVertice(this , id_vertex)
        class(grafo), intent(inout) :: this
        integer, intent(in) :: id_vertex
        type(vertice), pointer :: new_vertex, vertex_current
        if(.not. associated(ObtenerVertice(this, id_vertex)))then
            allocate(new_vertex)
            new_vertex%vertex_id = id_vertex ! id_vertice
            new_vertex%next_vertex => null() ! puntero next vertice
            new_vertex%ari => null()         ! puntero next ari

            if(Graph_is_empty(this))then
                this%first_vertex => new_vertex
            else
                vertex_current => this%first_vertex
                do while(associated(vertex_current%next_vertex)) 
                    vertex_current => vertex_current%next_vertex
                end do
                vertex_current%next_vertex => new_vertex
            end if
            this%zise_graph = this%zise_graph + 1 ! sumanos 1 al grafo
        else
            !print *, "Ya existe ese vertice"
        end if
    end subroutine InsertaVertice

    !Insertar una arista
    subroutine InsertArista(this, ori_name, dest_name, distance, print_mant)
        class(grafo), intent(inout) :: this
        integer, intent(in) :: ori_name, dest_name
        integer, intent(in) :: distance ! distancia ingresar al grafo
        integer, intent(in) :: print_mant ! impresora a mantenimiento
        type(vertice), pointer :: vertex_ori, vertex_dest
        type(arista), pointer :: new_ari, temp

        vertex_ori => ObtenerVertice(this, ori_name)
        vertex_dest => ObtenerVertice(this, dest_name) ! retorna un 

        if(.not. associated(vertex_ori))then
            print *, "No existe el vertice origen"
            return
        end if

        if(.not. associated(vertex_dest))then
            print *, "No existe el vertice destino"
            return
        end if

        allocate(new_ari)
        call initArista(new_ari, vertex_dest, distance, print_mant)

        if(.not. associated(vertex_ori%ari))then
            vertex_ori%ari => new_ari
        else
            temp => vertex_ori%ari
            do while(associated(temp%next_ari))
                temp => temp%next_ari
            end do
            temp%next_ari => new_ari
        end if
    end subroutine InsertArista

    !Mostrar Lista de adyacencia
    subroutine MostrarListaAdyacencia(this)
        class(grafo), intent(in) :: this
        type(vertice), pointer :: i
        type(arista), pointer :: j
    
        i => this%first_vertex  ! Iniciar con el primer vértice
    
        ! Recorrer todos los vértices del grafo
        do while (associated(i))    
            j => i%ari  ! Iniciar con la primera arista del vértice actual
    
            ! Recorrer todas las aristas del vértice actual
            do while (associated(j))
                
                write(*, '(a, a, a, a, a, a, a, a, a, a)', advance='no') trim(adjustl(writeInteger(i%vertex_id))) // &
                " -> ", "[", trim(adjustl(writeInteger(j%distance))) ,"]",&
                "[",trim(adjustl(writeInteger(j%print_mant))), "]", " -> ", &
                trim(adjustl(writeInteger(j%dest_vertex%vertex_id))) // ", "
                j => j%next_ari  ! Moverse a la siguiente arista
            end do
    
            print*, "" ! Nueva línea después de listar todas las aristas de un vértice
            i => i%next_vertex  ! Moverse al siguiente vértice
        end do
    end subroutine MostrarListaAdyacencia

    !Grafica del Grafo
    subroutine GenerarGrafoDot(this)
        class(grafo), intent(in) :: this
        type(vertice), pointer :: i
        type(arista), pointer :: j
        integer :: io, iostat, vertice_count
        character(len=200) :: command
        
        ! Abrir archivo para escribir el grafo en formato DOT
        open(newunit=io, file='grafo.dot', status='replace', action='write', iostat=iostat)
        
        if (iostat /= 0) then
            print *, "Error al abrir el archivo grafo.dot"
            return
        end if
    
        ! Escribir la configuración inicial del grafo
        write(io, *) "digraph G {"
        write(io, *) "rankdir=LR;"
        write(io, *) "nodesep=0.80;"
        write(io, *) "ranksep=2;"
        write(io, *) "node [shape=circle, style=filled, fillcolor=lightblue, fontcolor=black];"
        write(io, *) "edge [color=black, fontcolor=blue];"
    
        vertice_count = 0
        i => this%first_vertex
        ! Iterar sobre cada vértice y escribir su información y conexiones
        do while (associated(i))
            write(io, '("    ", a, " [label=""", a, """];")') trim(writeInteger(i%vertex_id)), trim(writeInteger(i%vertex_id))
            vertice_count = vertice_count + 1
    
            j => i%ari
            do while (associated(j))
                write(io, '("    ", a, " -> ", a, " [label=<", i0, "<br/><font color=''red''>", i0, "</font>>];")') &
                trim(writeInteger(i%vertex_id)), trim(writeInteger(j%dest_vertex%vertex_id)), j%distance, j%print_mant
                
                j => j%next_ari
            end do
    
            ! Cada dos vértices, generar un grupo de rango
            if (mod(vertice_count, 2) == 0) then
                if (associated(i%next_vertex)) then
                    write(io, '("    { rank=same; ", a, "; ", a, "; }")') &
                    trim(writeInteger(i%next_vertex%vertex_id)), trim(writeInteger(i%vertex_id))
                else
                    ! En caso de que i sea el último y no haya i%next_vertex , solo escribe i.
                    write(io, '("    { rank=same; ", a, "; }")') trim(writeInteger(i%vertex_id))
                end if
            end if

            i => i%next_vertex
        end do
    
        write(io, *) "}"
    
        close(io)
    
        ! Ejecutar Graphviz para convertir el archivo DOT en una imagen PNG
        call execute_command_line('dot -Tpng grafo.dot -o grafo.png', exitstat=iostat)
        command = 'start '// 'grafo'//'.png' !Abrir la imagen automaticamente
        call system(command)

        if (iostat == 0) then
            print *, "Imagen del grafo creada con éxito en 'grafo.png'."
        else
            print *, "Error al crear la imagen del grafo."
        end if
    end subroutine GenerarGrafoDot

    !Graph best route 
    subroutine graph_best_route(this, arr_rout)
        class(grafo), intent(in) :: this
        type(vertice), pointer :: i
        type(arista), pointer :: j
        integer, allocatable, intent(in) :: arr_rout(:)
        character(len=100) :: edge_color_index
        logical :: is_in_route , is_in_route_ari
        integer :: io, iostat, vertice_count
        integer :: k
        character(len=200) :: command

        ! Abrir archivo para escribir el grafo en formato DOT
        open(newunit=io, file='grafo_best_route.dot', status='replace', action='write', iostat=iostat)
        
        if (iostat /= 0) then
            print *, "Error al abrir el archivo grafo.dot"
            return
        end if
        
        ! Escribir la configuración inicial del grafo
        write(io, *) "digraph G {"
        write(io, *) "rankdir=LR;"
        write(io, *) "nodesep=0.80;"
        write(io, *) "ranksep=2;"
        write(io, *) "node [shape=circle, style=filled, fillcolor=lightblue, fontcolor=black];"
        write(io, *) "edge [color=black, fontcolor=blue];"
       
        vertice_count = 0
        i => this%first_vertex
        ! Iterar sobre cada vértice y escribir su información y conexiones
        do while (associated(i))
            is_in_route = any(arr_rout == i%vertex_id)
            if(is_in_route)then
                write(io, '("    ", a, " [label=""", a, """, style=filled, fillcolor=green];")') &
                trim(writeInteger(i%vertex_id)), trim(writeInteger(i%vertex_id))
                vertice_count = vertice_count + 1
            else
                write(io, '("    ", a, " [label=""", a, """];")') trim(writeInteger(i%vertex_id)), trim(writeInteger(i%vertex_id))
                vertice_count = vertice_count + 1
            end if
        
            j => i%ari
            do while (associated(j))
                
                do k = 1, size(arr_rout) - 1
                    if(arr_rout(k) == i%vertex_id .and. arr_rout(k+1) == j%dest_vertex%vertex_id)then
                        is_in_route_ari = .true.
                        exit
                    else
                        is_in_route_ari = .false.
                    end if
                end do
                if(is_in_route_ari) then
                    write(io, &
                    '("    ", a, " -> ", a, " [color=green, penwidth=2.0, label=<", i0, ' // &
                    '"<br/><font color=''red''>", i0, "</font>>];")') &
                    trim(writeInteger(i%vertex_id)), trim(writeInteger(j%dest_vertex%vertex_id)), j%distance, j%print_mant
                    j => j%next_ari
                else
                    write(io, '("    ", a, " -> ", a, " [label=<", i0, "<br/><font color=''red''>", i0, "</font>>];")') &
                    trim(writeInteger(i%vertex_id)), trim(writeInteger(j%dest_vertex%vertex_id)), j%distance, j%print_mant
                    j => j%next_ari
                end if
                
            end do
            
            ! Cada dos vértices, generar un grupo de rango
            if (mod(vertice_count, 2) == 0) then
                if (associated(i%next_vertex)) then
                    write(io, '("    { rank=same; ", a, "; ", a, "; }")') &
                    trim(writeInteger(i%next_vertex%vertex_id)), trim(writeInteger(i%vertex_id))
                else
                    ! En caso de que i sea el último y no haya i%next_vertex , solo escribe i.
                    write(io, '("    { rank=same; ", a, "; }")') trim(writeInteger(i%vertex_id))
                end if
            end if
            
            i => i%next_vertex
        end do
    

        write(io, *) "}"
    
        close(io)
    
        ! Ejecutar Graphviz para convertir el archivo DOT en una imagen PNG
        call execute_command_line('dot -Tpng grafo_best_route.dot -o grafo_best_route.png', exitstat=iostat)
        
        command = 'start '//'grafo_best_route'//'.png' !Visualizar la imagen
        call system(command)

        if (iostat == 0) then
            print *, ""
            print *, ":. Se genero la grafica con el camino mas optimo .:"
            print *, ""
        else
            print *, "Error al crear la imagen del grafo."
        end if
    end subroutine graph_best_route

    !Buscar un vertice por id
    function find_vertex_by_id(first_vertex, vertex_id) result(vertex_ptr)
        type(vertice), pointer :: first_vertex, vertex_ptr
        integer, intent(in) :: vertex_id
    
        vertex_ptr => first_vertex
        do while (associated(vertex_ptr))
            if (vertex_ptr%vertex_id == vertex_id) then
                return
            end if
            vertex_ptr => vertex_ptr%next_vertex
        end do
        nullify(vertex_ptr)  ! Si no se encuentra el vértice, devuelve un puntero nulo
    end function find_vertex_by_id

    ! ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    ! ::: Subrutinas para Rutas minimas y maximas Grafos 

    ! Insertar cola por la distance minima
    subroutine insert_priority_queue(this, v)
        class(cola_prioridad), intent(inout) :: this
        type(vertice), pointer, intent(in) :: v
        type(nodo_cola), pointer :: new, current, previus

        allocate(new)
        new%vertex => v
        new%next_tail_vertx => null()

        if (.not. associated(this%head))then
            this%head => new
            return
        end if

        current => this%head
        previus => null()
        
        
        do while(associated(current))
            if(current%vertex%acummulated_dis > v%acummulated_dis) exit
                ! Si distancia acumulada de current es menor al vertice a ingresar
                previus => current 
                current => current%next_tail_vertx
        end do

        if (associated(previus))then
            new%next_tail_vertx => current
            previus%next_tail_vertx => new
        else
            new%next_tail_vertx => this%head
            this%head => new
        end if
    end subroutine insert_priority_queue


    ! Insertar cola - Maxima impresoras mantenimiento
    subroutine insert_maximum_queue(this, v)    
        class(cola_prioridad), intent(inout) :: this
        type(vertice), pointer, intent(in) :: v
        type(nodo_cola), pointer :: new, current, previus

        allocate(new)
        new%vertex => v
        new%next_tail_vertx => null()

        if (.not. associated(this%head))then
            this%head => new
            return
        end if

        current => this%head
        previus => null() 
        
        !Ordenamos de cuardo a la maxima cantidad de impresorar a mantenimiento
        do while(associated(current)) 
            if(current%vertex%printMant_dis < v%printMant_dis) exit
                previus => current 
                current => current%next_tail_vertx
        end do

        if (associated(previus))then
            new%next_tail_vertx => current
            previus%next_tail_vertx => new
        else
            new%next_tail_vertx => this%head
            this%head => new
        end if
    end subroutine insert_maximum_queue


    subroutine extract_first_vertex(this, v) ! Extraer el vertice con el valor distancia minimo  o impresoras maximo 
        class(cola_prioridad), intent(inout) :: this
        type(vertice), pointer :: v
        type(nodo_cola), pointer :: temp

        if(.not. associated(this%head))then
            nullify(v)
            print *, "No hay nodos en la cola"
            return 
        end if  

        v => this%head%vertex ! obtiene el vertice
        temp => this%head
        this%head => this%head%next_tail_vertx ! cabeza apunta al siguiente

        deallocate(temp)

    end subroutine extract_first_vertex

    !Buscar todas las rutas del grafo
    subroutine find_all_routes(graph, origin_id, destination_id, all_routes)
        class(grafo), intent(inout) :: graph
        integer, intent(in) :: origin_id, destination_id
        type(vertice), pointer :: origin, destination
        type(nodo_vertice), pointer :: route_head
        type(ruta), pointer :: all_routes ! rutas
        logical :: first_call
       
        all_routes => null() ! head_route
        
        call initialize_parameters(graph) ! Incializamos los parametros 

        origin => ObtenerVertice(graph, origin_id)
        destination => ObtenerVertice(graph, destination_id)
    
        if (.not. associated(origin) .or. .not. associated(destination)) then 
            print *, "Origen o destino no encontrado."
            return
        endif
        
        first_call = .true. ! primera llamada del metodo
        allocate(route_head)
        route_head%vertice => origin
        route_head%next => null()
        ! Llamar a la subrutina recursiva para encontrar todas las rutasW
        call find_routes(graph, origin, destination, route_head, all_routes, 0 , 0 , first_call) ! buscar rutas

        !deallocate(path)
    end subroutine find_all_routes

    ! ::: Subrutina Buscar rutas ::: Esta ruta encontrara la ruta posible desde el orgien hasta el destino
    ! ::  Cuando llegue al destino guardar esta ruta    
    recursive subroutine find_routes(graph, current, destination, route_head, all_routes, total_dist, total_print_mant, first_call)
        type(grafo), intent(in) :: graph
        type(vertice), pointer :: current, destination
        type(nodo_vertice), pointer :: route_head, new_node, cloned_route_head, last_node
        type(vertice), pointer :: neighbor
        type(arista), pointer :: edge
        type(ruta), pointer :: new_route
        type(ruta), pointer :: all_routes
        type(nodo_vertice), pointer :: check_node

        integer :: total_dist, total_print_mant
        logical :: is_associated, first_call, is_route_added

        is_route_added = .false. ! bandera si esta agregado o no

        allocate(new_node)
        new_node%vertice => current
        new_node%next => null()


        if (first_call) then
            route_head => new_node
            first_call = .false.
        else
            last_node => route_head
            do while(associated(last_node%next))
                last_node => last_node%next
            end do
            last_node%next => new_node ! inserta el nuevo vertice o nodo
        end if

        if (associated(current) .and. current%vertex_id == destination%vertex_id) then
            ! Crear una nueva ruta
            allocate(new_route)
            new_route%vertice_head => route_head
            new_route%total_distancia = total_dist
            new_route%total_impresoras = total_print_mant
            new_route%next_route => null()
            call addRoute(all_routes, new_route) ! añadir ruta 
            is_route_added = .true.
        else
            edge => current%ari
            do while (associated(edge)) 
                neighbor => edge%dest_vertex
                is_associated = .false.
                ! Verificar si el vecino ya está en la ruta actual para evitar ciclos
                last_node => route_head 
                do while (associated(last_node))
                    if (last_node%vertice%vertex_id == neighbor%vertex_id) then
                        !ultimo%vertice%id_vertice
                        is_associated = .true. 
                        exit
                    end if
                    last_node => last_node%next
                end do

                if (.not. is_associated) then
                    cloned_route_head => clone_route_list(route_head)
                    call find_routes(graph, neighbor, destination, cloned_route_head, all_routes,&   ! aqui me lo va agregando 3->5->6->4
                        total_dist + edge%distance, total_print_mant + edge%print_mant, .false.)
                end if
                edge => edge%next_ari
            end do
        end if

            if (.not. is_route_added) then
                deallocate(new_node)
            end if
    end subroutine find_routes

    ! Clonar lista de la ruta (forma parte de find_routes)
    function clone_route_list(route_head) result(list_clone)
        type(nodo_vertice), pointer :: route_head
        type(nodo_vertice), pointer :: list_clone
        type(nodo_vertice), pointer :: current_vertex 
        type(nodo_vertice), pointer :: next_vertex
        type(nodo_vertice), pointer :: prev_clone
    
        list_clone => null()
        if (.not. associated(route_head)) return 
        
        allocate(current_vertex) 
        current_vertex%vertice => route_head%vertice 
        current_vertex%next => null() 
        list_clone => current_vertex ! head [1,7]
        prev_clone => current_vertex ! tail

        next_vertex => route_head%next ! Si tiene un siguiente en nodo_vertice

        do while (associated(next_vertex))
            allocate(current_vertex)  
            current_vertex%vertice => next_vertex%vertice
            current_vertex%next => null()
            prev_clone%next => current_vertex
            prev_clone => current_vertex 

            next_vertex => next_vertex%next
        end do
    end function clone_route_list

    function findMaxRoute(head) result(maxRout)
        type(ruta), pointer :: head, maxRout, current
        integer :: maxInpresoras

        maxRout => null()
        if(.not. associated(head)) return

        maxInpresoras = -1
        current => head

        do while(associated(current))
            if(current%total_impresoras > maxInpresoras)then
                maxInpresoras = current%total_impresoras
                maxRout => current
            end if
            current => current%next_route
        end do
    end function findMaxRoute
    
    subroutine best_maxRoute(all_routes, ruta_send, total_distancia, total_impresoras)
        type(ruta), pointer :: all_routes, rout_max
        type(nodo_vertice), pointer :: current_vertex
        type(vertice), pointer :: vertex
        !variables a retornar 
        integer, intent(out) :: total_distancia, total_impresoras
        integer, allocatable, intent(out) :: ruta_send(:)
        integer :: count, i
        integer :: costos, ganancias

        rout_max => findMaxRoute(all_routes)
        !inicializar costos y ganancias
        costos = 0
        ganancias = 0

        if(associated(rout_max))then
            current_vertex => rout_max%vertice_head

            count = 0
            do while(associated(current_vertex))
                count = count + 1
                current_vertex => current_vertex%next
            end do

            if(count > 0)then
                allocate(ruta_send(count))
                current_vertex => rout_max%vertice_head
                i = 1
                print*, "Ruta maxima impresoras"
                do while(associated(current_vertex))
                    vertex => current_vertex%vertice

                    if(associated(vertex))then
                        !asigno a mi ruta a retornar
                        ruta_send(i) = vertex%vertex_id 
                        write(* , '(A)', advance='no') trim(adjustl(writeInteger(vertex%vertex_id))) !escribir en consola la ruta
                        if(associated(current_vertex%next))then ! si hay un siguiente vertice agregar flecha
                            write(*, '(A)', advance='no') ' -> '
                        end if
                        i = i + 1 !sumar i para el indice
                    end if
                    current_vertex => current_vertex%next
                end do
            end if
            
            print *, ""
            print *, "Distancia recorrida: ", rout_max%total_distancia
            print *, "Cantida de impresorar reparadas: ", rout_max%total_impresoras
            costos = (rout_max%total_distancia)*80
            ganancias = (rout_max%total_impresoras)*100
            print *, "Costos: ", costos
            print *, "Ingresos extras: ", ganancias
            print *, ""
            total_distancia = rout_max%total_distancia
            total_impresoras = rout_max%total_impresoras
        else
            print *, "No se encontro la ruta maxima"
            return         
        end if
    end subroutine best_maxRoute 

    subroutine show_all_routes(all_routes)
        type(ruta), pointer :: all_routes
        type(ruta), pointer :: current_route
        type(nodo_vertice), pointer :: current_node
        type(vertice), pointer :: vertex
        integer :: route_count, vertex_count
    
        route_count = 0
        current_route => all_routes
        do while (associated(current_route))
            route_count = route_count + 1
            print *, "Ruta ", route_count, ":"
    
            vertex_count = 0
            current_node => current_route%vertice_head
            do while (associated(current_node))
                vertex_count = vertex_count + 1
                vertex => current_node%vertice
                print *, "Vértice ID: ", vertex%vertex_id
                current_node => current_node%next
            end do
    
            print *, "Total distancia de la ruta: ", current_route%total_distancia
            print *, "Total impresoras reparadas en la ruta: ", current_route%total_impresoras
            current_route => current_route%next_route
        end do
    
        if (route_count == 0) then
            print *, "No se encontraron rutas."
        endif
    end subroutine show_all_routes
    

    subroutine reset_grap_visits(this)
        type(grafo), intent(inout) :: this
        type(vertice), pointer :: v
        type(arista), pointer :: a

        v => this%first_vertex
        do while(associated(v))
            v%visited = .false.
            a => v%ari
            do while(associated(a))
                a%ari_visitada = .false.
                a => a%next_ari
            end do
            v => v%next_vertex
        end do
    end subroutine reset_grap_visits

    subroutine best_route(this, origen, destino, ruta ,total_distancia, total_impresoras, completed_successfully)    
        class(grafo), intent(inout) :: this
        integer, intent(in) :: origen , destino
        type(vertice), pointer :: current, vecino
        type(cola_prioridad) :: tail
        type(arista), pointer :: arista_actual 
        integer :: count 
        logical, intent(out) :: completed_successfully
        integer :: costos, ganancias 

        !variables a retornar - ruta - distancia - impresoras_mantenimiento
        integer, allocatable, intent(out) :: ruta(:)
        integer, intent(out) :: total_distancia, total_impresoras

        
        call initialize_parameters(this)
        costos = 0
        ganancias = 0

        current => ObtenerVertice(this, origen)
        if(.not. associated(current)) then 
            print *, "Vertice origen no encontrado."
            return
        end if

        current%acummulated_dis = 0 ! la distan del nodo origen esta en 0
        current%printMant_dis = 0
        current%parent_nodo => null()
        call tail%insert_priority_queue(current)

        do while(associated(tail%head))
            call tail%extract_first_vertex(current) ! obtenemos el vertice minimo
            
            if(associated(current))then
                if(current%vertex_id == destino)then
                    print *, "Rutas mas corta"
                    call show_route(current) ! obtenendo el nodo padre de la ruta 
                    print *, ""
                    print *, "Distancia del camino mas corto: ", current%acummulated_dis
                    print *, "Impresoras reparadas: ", current%printMant_dis
                    costos = (current%acummulated_dis)*80 !costo por la ruta minima
                    ganancias = (current%printMant_dis)*100 !ganancias por la ruta minima
                    print *, "Costos: ", costos
                    print *, "Ingresos Extras: ", ganancias
                    print *, ""
                    !valores a retornar
                    call get_route(current, ruta, count)
                    total_distancia = current%acummulated_dis
                    total_impresoras = current%printMant_dis
                    completed_successfully = .true.
                    return
                end if
            end if
            current%visited = .true.
            arista_actual => current%ari

            do while(associated(arista_actual)) 
                vecino => arista_actual%dest_vertex ! vertice venico o destino
                if(.not. vecino%visited)then
                    if(vecino%acummulated_dis > current%acummulated_dis + arista_actual%distance) then
                        vecino%acummulated_dis = current%acummulated_dis + arista_actual%distance
                        vecino%printMant_dis = current%printMant_dis + arista_actual%print_mant
                        vecino%parent_nodo => current
                        call tail%insert_priority_queue(vecino) ! insertar a la cola de prioridad
                    end if
                end if
                arista_actual => arista_actual%next_ari ! aqui  
            end do
        end do
        print *, "No se encontró camino al destino."
    end subroutine best_route

    subroutine best_route_maxPrintMant(this, origen, destino)    
        class(grafo), intent(inout) :: this
        integer, intent(in) :: origen , destino
        type(vertice), pointer :: current, vecino
        type(cola_prioridad) :: tail
        type(arista), pointer :: arista_actual 
    
       

        call initialize_parameters(this)
    
        current => ObtenerVertice(this, origen)
        if (.not. associated(current)) then 
            print *, "Vertice origen no encontrado."
            return
        end if
        
        current%acummulated_dis = 0
        current%printMant_dis = 0
        current%parent_nodo => null()
        call tail%insert_maximum_queue(current)
    
        do while(associated(tail%head))
            call tail%extract_first_vertex(current)
            
            if(associated(current))then
                if (current%vertex_id == destino) then
                    print *, "Ruta maximice la cantida de impresoras"
                    call show_route(current)
                    print *, ""
                    print *, "Distancia del camino: ", current%acummulated_dis
                    print *, "Maximo de impresoras reparadas: ", current%printMant_dis
                    return
                endif
            end if
    
            current%visited = .true.
            arista_actual => current%ari

            do while(associated(arista_actual))
                vecino => arista_actual%dest_vertex
                if(.not. vecino%visited)then
                    if (vecino%printMant_dis < current%printMant_dis + arista_actual%print_mant) then
                        vecino%printMant_dis = current%printMant_dis + arista_actual%print_mant
                        vecino%acummulated_dis = current%acummulated_dis + arista_actual%distance
                        vecino%parent_nodo => current   
                        call tail%insert_maximum_queue(vecino)
                    endif
                end if
                arista_actual => arista_actual%next_ari
            end do
        end do
        print *, "No se encontró camino al destino."
    end subroutine best_route_maxPrintMant
    

    
    ! Inicializar parametros del vertice 
    subroutine initialize_parameters(this)
        class(grafo), intent(inout) :: this
        type(vertice), pointer ::  current_vertex

        current_vertex => this%first_vertex

        do while(associated(current_vertex))
            current_vertex%acummulated_dis = 9999
            current_vertex%printMant_dis = 0
            current_vertex%visited = .false.
            current_vertex%parent_nodo => null()
            current_vertex => current_vertex%next_vertex
        end do
    end subroutine initialize_parameters

    !Mostrar ruta (se utiliza en best_route)
    subroutine show_route(destino)
        type(vertice), pointer :: destino, paso
        type(vertice), pointer :: ruta(:)   ! arreglo de puntero donde se va a guardar cada ruta
        integer :: count, i
    
        paso => destino !// 5
        count = 0
   
        ! Cuántos pasos se dieron para llegar al origen para darle tamaño a nuestro arreglo
        do while(associated(paso))
            count = count + 1
            paso => paso%parent_nodo
        end do
    
        allocate(ruta(count))
        paso => destino     
        i = count
    
        ! Almacenar la ruta en el arreglo para mostrarla desde el origen al destino
        do while(associated(paso))
            ruta(i) = paso
            i = i - 1
            paso => paso%parent_nodo
        end do
    
        do i = 1, count
            write(*, '(A, A)', advance='no') trim(adjustl(writeInteger(ruta(i)%vertex_id)))
            if (i < count) then
                write(*, '(A)', advance='no') ' -> '
            end if
        end do

        deallocate(ruta) ! desalogar el arreglo de puntero
    end subroutine show_route

    !Obtener la ruta y retornala
    subroutine get_route(destino, ruta, count)
        type(vertice), pointer :: destino, paso
        integer, allocatable ,intent(out) :: ruta(:)
        integer, intent(out) :: count
        integer :: i 
        paso => destino
        count = 0

        do while(associated(paso))
            count = count + 1
            paso => paso%parent_nodo
        end do

        allocate(ruta(count))
        paso => destino
        i = count

        do while(associated(paso))
            ruta(i) = paso%vertex_id
            i = i - 1
            paso => paso%parent_nodo
        end do

    end subroutine get_route
    
    !Subrutina para obtener la mejor ruta 
    subroutine optimal_route(this, origen, destino, id_tecnico, bst_tree, json, my_block_chain, my_all_list_sucursales)
        class(grafo), intent(inout) :: this
        type(bst), intent(in) :: bst_tree
        type(ruta), pointer :: all_routes
        type(tree_merkle) :: merkle ! Al finalizar la ejecución de la subrutina, cualquier dato almacenado en esta instancia se perderá si no se ha guardado o retornado de alguna manera.
        integer, intent(in) :: origen, destino
        integer*8, intent(in) :: id_tecnico
        type(list_sucursales), intent(inout) :: my_all_list_sucursales

        integer, allocatable :: ruta_dis(:) !Ruta minimizando distancia
        integer, allocatable :: ruta_print(:) !Ruta maximizando impresoras
        integer, allocatable :: ruta_optimal(:) !Mejor Ruta

        integer :: distancia_rout1, totalImpresoras_rout1 ! total distancia y impresora - minima distancia
        integer :: distancia_rout2, totalImpresoras_rout2 ! total distancia y impresora - maxima impresoras
        logical :: successfully
        integer :: i, j

        integer, parameter :: costo_distancia = 80 ! costo de la distancia por kilometros
        integer, parameter :: costo_reparacion = 100 !ingresos por reparacion de impresoras

        integer :: total1 = 0 !total1 = (totalImpresoras_rout1*100)-(distancia_rout1*80)
        integer :: total2 = 0 !total2 = (totalImpresoras_rout2*100)-(distancia_rout2*80)

        !variables para almacenar la infromacion 
        type(node_sucursal), pointer :: sucursal_info, next_sucursal_info
        type(vertice), pointer :: current_vertex
        type(arista), pointer :: current_edge !actual arista 
        integer :: route_distance ! costo total de la arista entre dos sucursales
        character(len=:), allocatable :: info_concatenada
        integer index
        
        !Variables y tipos para crear el Json de los bloques
        type(json_core), intent(inout) :: json
        type(json_value), pointer :: root_array, new_data_object, data_array, path
        character(len=:), allocatable :: root_merkle
        character(len=:), allocatable :: dataBlock_Chain !INFORMACION DEL HASH (INDEX, TIME 
                                                        !PREVIUS HASH, ROOTMERKLE, NONE)
        character(len=:), allocatable :: hash_dataBlock

        !Instancias de mi lista block chain
        type(block_chain), intent(inout) :: my_block_chain
        type(all_data) :: my_data_block
        character(len=20) :: nonce


        logical :: file_exists
        !Variables para crear el timestamp
        character(len=8) :: date 
        character(len=10) :: time
        character(len=5) :: zone
        character(len=30) :: timestamp
        character(len=4) :: year
        character(len=2) :: month, day, hour, minute, second
        character(len=6) :: fraction 

        inquire(file='blockchain\blockchain.json', exist = file_exists )

        nullify(all_routes) ! nulificamos nuestro puntero all_routes 

        ! LLAMAMOS A LA FUNCION BUSCAR MEJOR RUTA POR DISTANCIA
        call this%best_route(origen, destino, ruta_dis, distancia_rout1, totalImpresoras_rout1, successfully)
        
        ! LLAMAMOS A LA FUNCION BUSCAR MEJOR RUTA POR CANTIDAD IMPREOSARAS
        call this%find_all_routes(origen, destino, all_routes)
        call best_maxRoute(all_routes, ruta_print, distancia_rout2, totalImpresoras_rout2 )
    
        if (successfully) then ! SI SE ENCUENTRA LA RUTA MINIMA
            total1 = (totalImpresoras_rout1 * costo_reparacion) - (distancia_rout1 * costo_distancia)
            total2 = (totalImpresoras_rout2 * costo_reparacion) - (distancia_rout2 * costo_distancia)
            
            print *, "--------------------------------------------------------" 
            print *, "total 1 - GANANCIAS POR DISTANCIA MINIMA:", total1
            print *, "total 2 - GANANCIAS MAXIMO NUMERO DE IMPRESORAS REPARADAS:", total2
            print *, "--------------------------------------------------------"

            !COMPARA LA DOS RUTAS (PESO: DISTANCIA) (PESO: CANTIDA IMPORESORAS)
            if(total1 > total2)then
                call this%graph_best_route(ruta_dis)
                allocate(ruta_optimal(size(ruta_dis)))
                ruta_optimal = ruta_dis

                !Incrementar los costos y ganancias si es para la ruta distancia minima
                costos_totales = costos_totales + (distancia_rout1 * costo_distancia)
                ingresos_extras_totales = ingresos_extras_totales + (totalImpresoras_rout1 * costo_reparacion)
                ganancias_totales = ganancias_totales + total1

            else
                call this%graph_best_route(ruta_print)
                allocate(ruta_optimal(size(ruta_print)))
                ruta_optimal = ruta_print

                !Incrementar los costos, ingresos y ganancias si es para la ruta con mayor numero de impresoras
                costos_totales = costos_totales + (distancia_rout2 * costo_distancia)
                ingresos_extras_totales = ingresos_extras_totales + (totalImpresoras_rout2 * costo_reparacion)
                ganancias_totales = ganancias_totales + total2
            end if


            !SI EXISTE ES ARCHIVO JSON DEL BLOCHAIN
            if(file_exists) then
                call json%load('C:\Users\Aitan\OneDrive\Escritorio\FASE3 EDD\blockchain\blockchain.json', root_array)
                call json%create_object(new_data_object, '')
                call json%create_array(data_array, 'DATA')
            else
                call json%create_array(root_array, '')
                call json%create_object(new_data_object, '')
                call json%create_array(data_array, 'DATA')
            end if

            !INGRESO LA RUTA OPTIMA A MI LISTA DE SUCURSALES CON MAS TRABAJOS REALIZADOS
            do index = 1, size(ruta_optimal)     
                call my_all_list_sucursales%find_one_list_sucursal(ruta_optimal(index))
            end do
            
            !OBTENER INFO PARA MI NODOS HOJAS DE MI ARBOL MERKLE & INFORMACION PARA MI BLOCKCHAIN
            do index = 1, size(ruta_optimal) - 1
                
                sucursal_info => bst_tree%search_node_bst_id(ruta_optimal(index)) !obtener info sucursal primera     
                next_sucursal_info => bst_tree%search_node_bst_id(ruta_optimal(index + 1)) !obtener info sucursal segunda posicion

                current_vertex => find_vertex_by_id(this%first_vertex, ruta_optimal(index)) !obtener el primer vertice de la ruta

                if(associated(current_vertex))then !Encontro vertice inial
                    current_edge => current_vertex%ari
                    do while(associated(current_edge))
                        if(current_edge%dest_vertex%vertex_id == ruta_optimal(index + 1))then
                            route_distance = (current_edge%distance) * 80 ! distancia * costo distancia
                            exit
                        end if
                        current_edge => current_edge%next_ari
                    end do
                end if

                if(associated(sucursal_info) .and. associated(next_sucursal_info))then
                    !Insertar en mi json la data de cada ruta optima
                    call json%create_object(path, '')
                    call json%add(path, 'sucursal_o', trim(adjustl(writeInteger(ruta_optimal(index)))))
                    call json%add(path, 'dirrecion_o', trim(sucursal_info%dirrecion)//','//trim(sucursal_info%departament))
                    call json%add(path, 'sucursal_d', trim(adjustl(writeInteger(ruta_optimal(index + 1)))))
                    call json%add(path, 'dirrecion_d', trim(next_sucursal_info%dirrecion)//','&
                                                     //trim(next_sucursal_info%departament))
                    call json%add(path, 'costo:', trim(adjustl(writeInteger(route_distance))))
                    call json%add(data_array, path)

                    !INFORMACION PARA SETEAR A MIS NODOS HOJAS ARBOL MERKLE
                    info_concatenada = ""
                    info_concatenada = "ID Sucursal Origen: " // trim(adjustl(writeInteger(ruta_optimal(index)))) // "\n "// & 
                    "Dirección: " // trim(sucursal_info%dirrecion) // "\n "// &
                    "ID Sucursal Destino: " // trim(adjustl(writeInteger(ruta_optimal(index + 1)))) // "\n "// &
                    "Dirección: " // trim(next_sucursal_info%dirrecion)// "\n "//& 
                    "Costo total: " // trim(adjustl(writeInteger(route_distance)))

                    !INSERTAR DATA EN MI LISTA DATA 
                    call my_data_block%add_data(trim(adjustl(writeInteger(ruta_optimal(index)))), &
                                                trim(sucursal_info%dirrecion)//','//trim(sucursal_info%departament), &
                                                trim(adjustl(writeInteger(ruta_optimal(index + 1)))), &
                                                trim(next_sucursal_info%dirrecion)//','//trim(next_sucursal_info%departament), &
                                                trim(adjustl(writeInteger(route_distance))))
                    

                end if
                call merkle%add(info_concatenada) !AÑADIR INFO A MI NODO HOJA DEL ARBOL MERKLE
            end do
            
            call merkle%generate() !GENERAMOS EL ARBOL MERKLE
            call merkle%grap_Tree_merkle() !GRAFICAMOS EL ARBOL MERKLE


            index_dataBlock = index_dataBlock + 1 !INCREMENTAR EL ÍNDICE EN DATABLOCK 
            call DATE_AND_TIME(date, time, zone)
            year = date(1:4)
            month = date(5:6)
            day = date(7:8)
            hour = time(1:2)
            minute = time(3:4)
            second = time(5:6)
            fraction = time(7:)

            timestamp = trim(year) // '-' // trim(month) // '-' // trim(day) & 
                //' ' // trim(hour) // '-' // trim(minute) // '-' // trim(second) &
                //trim(fraction) // ' ' // trim(zone)

            !GENERAR EL ROOT_MERKLE
            root_merkle = merkle%get_root_hash()

            !COMPROVAMOS SI TENEMOS UN ROOT HASH VALIDO PARA NUESTRO BLOQUE
            if(trim(root_merkle) == "")then
                print *, "No se puede generar un hash valido para el Arbol de merkle"
                return ! SALIR DE LA SUBRUTINA 
            end if

            !VALIDACIONES PARA COMPROBRAS SI ES EL PRIMER BLOQUE
            if(.not. is_first_previusHash)then
                previus_hash = "0000"
                is_first_previusHash = .true.
            else
                previus_hash = last_hash_dataBloke
            end if 
            
            !GENERAR EL HASH DEL BLOCKCHAIN
            dataBlock_Chain = trim(adjustl(writeInteger(index_dataBlock)))// trim(adjustl(timestamp)) &
                            // trim(adjustl(previus_hash))//trim(adjustl(root_merkle)) &
                            // trim(adjustl('0000'//trim(adjustl(writeInteger(index_dataBlock)))))
            
            hash_dataBlock = sha256(dataBlock_Chain)
            last_hash_dataBloke = hash_dataBlock
     

            call json%add(new_data_object, 'INDEX: ', index_dataBlock) !Insertar un index en DATABLOCK 
            call json%add(new_data_object, 'TIMESTAMP: ', timestamp) !Insertar TIMESTAMP en DATABLOCK
            call json%add(new_data_object, data_array)
            call json%add(new_data_object, 'NONCE:', '0000'//trim(adjustl(writeInteger(index_dataBlock))))
            call json%add(new_data_object, 'ROOT MERKLE:', root_merkle)
            call json%add(new_data_object, 'HASH: ', hash_dataBlock)
            call json%add(new_data_object, 'PREVIUS HASH: ', previus_hash)
            call json%add(root_array, new_data_object)
            call json%print(root_array, 'C:\Users\Aitan\OneDrive\Escritorio\FASE3 EDD\blockchain\blockchain.json')
            call json%destroy(root_array)

            !INICIALIZAR NONCE PARA MI LISTA
            nonce = ''
            nonce = '0000'//trim(adjustl(writeInteger(index_dataBlock)))
         
            !INSETAR BLOQUE EN MI LISTA 
            call my_block_chain%add_block(index_dataBlock, timestamp, my_data_block, nonce, &
                                          previus_hash, root_merkle, hash_dataBlock)    
            
        else
            print *, "No se pudo encontrar una ruta para la distancia minima."
        endif
     
        if (allocated(ruta_dis)) then
            deallocate(ruta_dis)
        endif
        
        if(allocated(ruta_print))then
            deallocate(ruta_print)
        end if

        if(allocated(ruta_optimal))then
            deallocate(ruta_optimal)
        end if
        
    end subroutine optimal_route
    
    ! ::::::::::: Subrutinas y funciones para Tabla Hash ::::::::::::::::::::::::::::::::
    !Inicializar tabla hash
    subroutine init(this, new_size, maxi)
        class(hash), intent(inout) :: this
        integer, intent(in) :: new_size, maxi
        integer :: i 

        this%elements = 0
        this%size_table = new_size
        this%maxi = maxi
    
        if(allocated(this%table))then
            deallocate(this%table)
        end if
        allocate(this%table(0:new_size-1))

        ! inicializate table 
        do i = 0, new_size-1
            this%table(i)%dpi = -1
            this%table(i)%name = ''
            this%table(i)%last_name = ''
            this%table(i)%genre = ''
            this%table(i)%direccion = ''
            this%table(i)%phone = 0
        end do

    end subroutine init

    !Obtener la posicision
    integer*8 function division(this, dpi)
        class(hash), intent(inout) :: this
        integer*8,  intent(in) :: dpi
        division = mod(dpi, this%size_table) ! dpi MOD size_table
    end function division
    
    !Obtener la posicion si hay una colision
    integer*8 function linear_probe(this, dpi, i)
        class(hash), intent(inout) :: this
        integer*8, intent(in) :: dpi
        integer, intent(in) :: i ! contador de colisiones 
        
        linear_probe = mod((mod(dpi, 7) + 1) * i, this%size_table)
        
    end function linear_probe

    !insertar tabla hash
    subroutine insert(this, tech)
        class(hash), intent(inout) :: this
        type(techinical), intent(in) :: tech
        integer*8 :: pos, initial_pos
        integer :: i

        pos = this%division(tech%dpi) ! get this position  insert element
        initial_pos = pos ! save de position initial
        i = 0 ! se reinicia cada vez que se inserta un elemento 
        do while(this%table(pos)%dpi /= -1)
            i = i + 1
            pos = this%linear_probe(tech%dpi, i)
        end do 

        this%table(pos) = tech 
        this%elements = this%elements + 1
        call this%reashing()
    end subroutine insert

    !Reashing table hash
    subroutine reashing(this)
        class(hash), intent(inout) :: this
        integer :: i, prev_size
        type(techinical), dimension(:), allocatable :: temp
        
        if(this%elements * 100 / this%size_table >= this%maxi) then
            allocate(temp(this%size_table))
            temp = this%table ! tabal temporal

            call this%show()

            prev_size = this%size_table
            this%size_table = this%size_table * 2

            call this%init(this%size_table, this%maxi)
            do i = 1 , prev_size
                if(temp(i)%dpi /= -1)then
                    call this%insert(temp(i))
                end if
            end do
        else
            call this%show()
        end if
    end subroutine reashing

    !Mostrar tabla hash
    subroutine show(this)
        class(hash), intent(in) :: this
        integer :: i 
        write (*, '(a)', advance='no') '['
        do i = 0 , this%size_table - 1
            write(*, '(i13, A)', advance='no') this%table(i)%dpi, "  "
        end do
        write(*, '(A, I0, A)') '] ', (this%elements * 100 / this%size_table), '%'
    end subroutine show
    
    ! Grafica tabla Hash
    subroutine graph_hash_table(this, filename)
        class(hash), intent(in) :: this
        character(len=*), intent(in) :: filename
        integer :: i, unit, exitstat, j
        character(len=100) :: dot_filename, png_filename
        character(len=200) :: dot_command
        character(len=500) :: label_info
        character(len=20) :: client_id, port_label
        character(len=200) :: command
    
        dot_filename = trim(filename) // ".dot"
        png_filename = trim(filename) // ".png"
        dot_command = "dot -Tpng " // trim(dot_filename) // " -o " // trim(png_filename)
    
        open(newunit=unit, file=dot_filename, status='replace', action='write')
        write(unit, '(A)') "digraph G {"
        write(unit, '(A)') "rankdir=""LR"";"
        write(unit, '(A)') "node [style=filled, fillcolor=lightskyblue, shape=rect];"
        write(unit, '(A)') "parent [label=<<table cellborder='1' cellpadding='5'>]"
    
        if(allocated(this%table)) then
            do i = 0, this%size_table - 1
                if(this%table(i)%dpi /= -1) then
                    write(unit, '(A, I0, A, I0, A)') "<tr><td port='port_", i, "'>", i, "</td></tr>"
                else
                    write(unit, '(A, I0, A)') "<tr><td>", i, "</td></tr>"
                endif
            end do
        endif
    
        write(unit, '(A)') "</table>>];"
    
        if(allocated(this%table)) then
            do i = 0, this%size_table - 1
                if(this%table(i)%dpi /= -1) then
                    client_id = "cliente" // trim(adjustl(writeInteger(i)))
                    port_label = "parent:port_" // trim(adjustl(writeInteger(i)))
                    label_info = client_id // ' [label="' // "DPI: " //trim(adjustl(str__to_integer(this%table(i)%dpi)))// '\n' // &
                                 "Name: " // trim(this%table(i)%name) // " " // trim(this%table(i)%last_name) // '\n' // &
                                 "Genre: " // trim(this%table(i)%genre) // '\n' // &
                                 "Address: " // trim(this%table(i)%direccion) // '\n' // &
                                 "Phone: " // trim(adjustl(writeInteger(this%table(i)%phone))) // '"];'
                    write(unit, '(A)') label_info
                    write(unit, '(A, A)') client_id // " -> ", port_label // ";"
                endif
            end do
        endif
    
        write(unit, '(A)') "}"
    
        close(unit)
    
        call execute_command_line(dot_command, exitstat=j)
        
        command = 'start '// trim(filename) // '.png' !Mostrar la imagen automaticamente
        call system(command)

        if(exitstat /= 0) then
            print *, "Error al crear la grafica"
        else
            print *, ":. Se creo correctamente la grafica de la tabla hash .:"
        endif
    end subroutine graph_hash_table
    
    !funcion convertir integer*8 a str 
    function str__to_integer(num) result(str_integr) !function para convertir un entero a un String 
        integer*8, intent(in) :: num
        character(len=32) :: str_integr
        write(str_integr, '(I0)') num
        str_integr = adjustl(trim(str_integr))
    end function str__to_integer


    !Buscar un tecnico - Si lo encuentra sumarle 1 a jobs_done y retornar true
    function find_technician(this, dpi_tech) result(found_tech)
        class(hash), intent(inout) :: this
        integer*8, intent(in) :: dpi_tech
        integer :: i 
        logical :: found_tech
        found_tech = .false.

        do i = 0 , this%size_table - 1
            if(this%table(i)%dpi == dpi_tech)then
                this%table(i)%jobs_done = this%table(i)%jobs_done + 1
                found_tech = .true.
                return
            end if
        end do

    end function find_technician

    !Buscar un tecnico por el DPI
    subroutine find_technician_id(this, dpi_tech)
        class(hash), intent(in) :: this
        integer*8, intent(in) :: dpi_tech
        integer :: i
        logical :: found
        
        found = .false.

        do i = 0, this%size_table - 1
            if(this%table(i)%dpi == dpi_tech)then
                print*, ""
                print*, "-- Informacion del tecnico --"
                write(*, '(A, I15)') "DPI: ", this%table(i)%dpi
                write(*, '(A, A)') "Nombre: ", trim(this%table(i)%name)
                write(*, '(A, A)') "Apellido: ", trim(this%table(i)%last_name)
                write(*, '(A, A)') "Genero: ", trim(this%table(i)%genre)
                write(*, '(A, A)') "Dirrecion: ", trim(this%table(i)%direccion)
                write(*, '(A, I0)') "Telefono: ", this%table(i)%phone
                write(*, '(A, I0)') "Trabajos realizados: ", this%table(i)%jobs_done
                print*, "-----------------------------"
                print*, ""
                found = .true.
                exit ! salir una vez lo haya encontrado
            end if
        end do

        if(.not. found) then
            print*, "No existe el tecnico con el id: ", dpi_tech
        end if

    end subroutine find_technician_id

    !Listar tecnicos
    subroutine list_all_technician(this)
        class(hash), intent(in) :: this
        integer :: i

        print*, ""
        print*, "-- Listado de tecnicos --"
        do i = 0, this%size_table - 1
            if(this%table(i)%dpi /= -1)then
                print*, "-----------------------------"
                write(*, '(A, I15)') "DPI: ", this%table(i)%dpi
                write(*, '(A, A)') "Nombre: ", trim(this%table(i)%name)
                write(*, '(A, A)') "Apellido: ", trim(this%table(i)%last_name)
            end if
        end do
        print*, "-----------------------------"
        print*, ""
    end subroutine list_all_technician

    !:::::::::::: Subrutinas y funciones para añadir a la Lista auxiliar tecnicos 
    subroutine add_techical(this, dpi, name, last_name)
        class(all_techinical), intent(inout) :: this
        integer*8, intent(in) :: dpi
        character(len=:), allocatable, intent(in) :: name, last_name
        type(node_techinical), pointer :: new_tech, current_tech

        allocate(new_tech)
        new_tech%dpi = dpi
        new_tech%name = name
        new_tech%last_name = last_name
        new_tech%jobs_done = 0
        new_tech%next_techincal => null()

        if(.not. associated(this%head_techincal))then
            this%head_techincal => new_tech
        else
            current_tech => this%head_techincal
            do while(associated(current_tech%next_techincal))
                current_tech => current_tech%next_techincal
            end do
            current_tech%next_techincal => new_tech
        end if
    end subroutine add_techical

    !SUBRUTINA PARA BUSCAR TECNICO
    subroutine find_one_techical(this, dpi)
        class(all_techinical), intent(inout) :: this
        integer*8, intent(in) :: dpi
        logical :: found
        type(node_techinical), pointer :: current_tech

        found = .false.

        current_tech => this%head_techincal
        do while(associated(current_tech))
            if(current_tech%dpi == dpi)then
                current_tech%jobs_done = current_tech%jobs_done + 1
                found = .true.
                exit 
            end if
            current_tech => current_tech%next_techincal
        end do

        if(.not. found)then
            print*, "No se encontro el tecnico con el DPI:", dpi
        end if

    end subroutine find_one_techical

    !Ordenamiento burbuja ordenando los tecnicos por trabajo realizado
    subroutine bublle_sort_tech(this)
        class(all_techinical), intent(inout) :: this
        type(node_techinical), pointer :: current_tech, next_tech
        integer :: temp_job_done
        integer*8 :: temp_dpi
        character(len=32) :: temp_name, temp_last_name

        if(.not. associated(this%head_techincal))then
            print*, "No hay tecnicos en la lista"
            return
        end if

        current_tech => this%head_techincal
        do while(associated(current_tech) .and. associated(current_tech%next_techincal))
            next_tech => current_tech%next_techincal
            do while(associated(next_tech))
                if(current_tech%jobs_done < next_tech%jobs_done)then
                    !Intercambiar trabajos realizados
                    temp_job_done = current_tech%jobs_done
                    current_tech%jobs_done = next_tech%jobs_done
                    next_tech%jobs_done = temp_job_done
    
                    !Intercambiar dpi
                    temp_dpi = current_tech%dpi
                    current_tech%dpi = next_tech%dpi
                    next_tech%dpi = temp_dpi
    
                    !Intercambiar nombre 
                    temp_name = current_tech%name
                    current_tech%name = next_tech%name
                    next_tech%name = temp_name
    
                    !Intercambiar apellido
                    temp_last_name = current_tech%last_name
                    current_tech%last_name = next_tech%last_name
                    next_tech%last_name = temp_last_name
                end if
                next_tech => next_tech%next_techincal
            end do
            current_tech => current_tech%next_techincal
        end do
    end subroutine bublle_sort_tech

    !Mostrar top 5 tenicos con mayor numeros de trabajos realizados
    subroutine top_5_tech(this)
        class(all_techinical), intent(in) :: this
        type(node_techinical), pointer :: current_tech
        integer :: count

        current_tech => this%head_techincal
        count = 0
        print *, "Top 5 tecnicos con mayor numero de trabajos realizados"
        print *,"-------------------------------------"
        do while(associated(current_tech) .and. count < 5)
            write(*, '(A, I20)') 'DPI del tecnico: ', current_tech%dpi
            write(*, '(A, A, A, A)') &
            'Nombre del tecnico: ', trim(adjustl(current_tech%name)), "  ", trim(adjustl(current_tech%last_name))
            write(*, '(A, I0)') 'Trabajos realizados: ', current_tech%jobs_done
            count = count + 1
            print *, "-------------------------------------"
            current_tech => current_tech%next_techincal
        end do
    end subroutine top_5_tech

    !Mostrar todos los tecnicos
    subroutine show_all_technicians(this)
        class(all_techinical), intent(in) :: this
        type(node_techinical), pointer :: current_tech
    
        current_tech => this%head_techincal
    
        if (.not. associated(current_tech)) then
            print *, "No hay técnicos para mostrar."
            return
        end if
    
        print *, "Mostrando todos los técnicos registrados:"
        do while(associated(current_tech))
            print *, "DPI:", current_tech%dpi
            print *, "Nombre:", trim(current_tech%name)
            print *, "Apellido:", trim(current_tech%last_name)
            print *, "Trabajos realizados:", current_tech%jobs_done
            print *, "-------------------------"
            current_tech => current_tech%next_techincal
        end do
    end subroutine show_all_technicians

    !:::::::::::: Subrutinas y funciones para añadir a la Lista de sucursales
    !Subrutina para añadir un nodo sucursal a la lista 
    subroutine add_node_list_suc(this, id_suc, departamento, direccion)
        class(list_sucursales), intent(inout) :: this
        integer, intent(in) :: id_suc
        character(len=:), allocatable , intent(in) :: departamento, direccion
        type(node_list_sucursal), pointer :: new_node, current

        allocate(new_node)
        new_node%id_sucursal = id_suc
        new_node%departament = departamento
        new_node%direccion = direccion
        new_node%next_suc => null()

        if(.not. associated(this%head_list))then
            this%head_list => new_node
        else
            current => this%head_list
            do while(associated(current%next_suc))
                current => current%next_suc
            end do
            current%next_suc => new_node
        end if
    end subroutine add_node_list_suc

    !Subrutina para buscar un nodo sucursal en la lista y actualizarme request_jobs
    subroutine find_one_list_sucursal(this, id_sucursal_ori)
        class(list_sucursales), intent(inout) :: this
        integer, intent(in) :: id_sucursal_ori
        logical :: found
        type(node_list_sucursal), pointer :: current_node_suc
        
        found = .false.
        current_node_suc => this%head_list
        do while(associated(current_node_suc))
            if(current_node_suc%id_sucursal == id_sucursal_ori)then
                current_node_suc%request_jobs = current_node_suc%request_jobs + 1
                found = .true.
                exit
            end if
            current_node_suc => current_node_suc%next_suc
        end do

        if(.not. found)then
            print*, "No se encontro la sucursal con el id de origen", id_sucursal_ori
        end if
    end subroutine find_one_list_sucursal

    !Subrutina para ordenar una sucursal por medio de request jobs
    subroutine bublle_sort_list_suc(this)
        class(list_sucursales), intent(inout) :: this
        type(node_list_sucursal), pointer :: current_suc, next_sucursal
        integer :: temp_request_jobs
        integer :: temp_id_sucursal
        character(len=32) :: temp_departament, temp_dirrecion
    
        if (.not. associated(this%head_list)) then
            print*, "No hay sucursales en la lista."
            return
        end if
    
        current_suc => this%head_list
        do while(associated(current_suc) .and. associated(current_suc%next_suc))
            next_sucursal => current_suc%next_suc
            do while(associated(next_sucursal))
                if (current_suc%request_jobs < next_sucursal%request_jobs) then
                    ! Intercambiar request_jobs
                    temp_request_jobs = current_suc%request_jobs
                    current_suc%request_jobs = next_sucursal%request_jobs
                    next_sucursal%request_jobs = temp_request_jobs
    
                    ! Intercambiar id_sucursal
                    temp_id_sucursal = current_suc%id_sucursal
                    current_suc%id_sucursal = next_sucursal%id_sucursal
                    next_sucursal%id_sucursal = temp_id_sucursal
    
                    ! Intercambiar departament
                    temp_departament = current_suc%departament
                    current_suc%departament = next_sucursal%departament
                    next_sucursal%departament = temp_departament
    
                    ! Intercambiar direccion
                    temp_dirrecion = current_suc%direccion
                    current_suc%direccion = next_sucursal%direccion
                    next_sucursal%direccion = temp_dirrecion
                end if
                next_sucursal => next_sucursal%next_suc
            end do
            current_suc => current_suc%next_suc
        end do
    end subroutine bublle_sort_list_suc

    !Subrutina para mostrar top_5_sucursales
    subroutine top_5_sucusales(this)
        class(list_sucursales), intent(in) :: this
        type(node_list_sucursal), pointer :: current_suc
        integer :: count
    
        current_suc => this%head_list
        count = 0
        print *, "Top 5 sucursales con mayor numero de trabajos solicitados"
        print *, "-------------------------------------"
        do while(associated(current_suc) .and. count < 5)
            write(*, '(A, I5)') 'ID de la sucursal: ', current_suc%id_sucursal
            write(*, '(A, A)') 'Departamento: ', trim(adjustl(current_suc%departament))
            write(*, '(A, A)') 'Dirección: ', trim(adjustl(current_suc%direccion))
            write(*, '(A, I0)') 'Trabajos solicitados: ', current_suc%request_jobs
            count = count + 1
            print *, "-------------------------------------"
            current_suc => current_suc%next_suc
        end do
    end subroutine top_5_sucusales

    !Subrutina para mostrar toda la lista de sucursales
    subroutine show_all_list_sucursales(this)
        class(list_sucursales), intent(in) :: this
        type(node_list_sucursal), pointer :: current_suc
    
        current_suc => this%head_list
    
        if (.not. associated(current_suc)) then
            print *, "No hay sucursales para mostrar."
            return
        end if
    
        print *, "Mostrando todas las sucursales registradas:"
        do while(associated(current_suc))
            print *, "ID Sucursal:", current_suc%id_sucursal
            print *, "Departamento:", trim(current_suc%departament)
            print *, "Dirección:", trim(current_suc%direccion)
            print *, "Trabajos solicitados:", current_suc%request_jobs
            print *, "-------------------------"
            current_suc => current_suc%next_suc
        end do
    end subroutine show_all_list_sucursales


    !::::::::::::: Subrutina y funciones para escribir la sucursal en el json

    !Funcion para encriptar la contrasena 
    function encriptar_contrasena(password) result(contrasena_encriptada)
        character(len=*), intent(in) :: password
        character(len=32) :: contrasena_encriptada
        integer :: i, code
        real :: semilla = 1

        contrasena_encriptada = ''  
        do i = 1, len_trim(password)
            code = ichar(password(i:i)) + nint(semilla) 
            contrasena_encriptada = trim(contrasena_encriptada) // achar(mod(code, 127))
        end do
    end function encriptar_contrasena

    !Subrutinas para escribir mis sucursales 
    recursive subroutine  add_bst_nodes_to_json(node, data, json)
        type(node_sucursal), pointer :: node
        type(json_value), pointer :: data, path
        type(json_core) :: json
        character(len=32) :: password_encrypted

        if(.not. associated(node)) return
        
        call add_bst_nodes_to_json(node%left, data, json)

        password_encrypted = encriptar_contrasena(node%password)

        call json%create_object(path, '')
        call json%add(path, 'id_sucursal', node%id_sucursal)
        call json%add(path, 'password', trim(password_encrypted))
        call json%add(path, 'departament', trim(node%departament))
        call json%add(path, 'direccion', trim(node%dirrecion))
        call json%add(data, path) 

        call add_bst_nodes_to_json(node%right, data, json)
        
    end subroutine add_bst_nodes_to_json

    !Escribir el arbol bst al json
    subroutine write_bst_to_json(this)
        class(bst), intent(in) :: this
        type(json_core) :: json
        type(json_value), pointer :: root, data1

        call json%initialize()
        call json%create_object(root, '')
        call json%create_array(data1, 'sucursales')

        call add_bst_nodes_to_json(this%root, data1, json)

        call json%add(root, data1)
        call json%print(root, "C:\Users\Aitan\OneDrive\Escritorio\FASE3 EDD\data_empresa\Sucursales_data.json")
        call json%destroy(root)
    end subroutine write_bst_to_json
    
    !:::::::: END Subrutina y funciones para escribir
 

    !::::::::::::::::::::::::::::   END Subrutinas y funciones para la Tabla Hash :::::::::::::

    
    ! Subrutinas para las sucursales 
    subroutine add_bst(this, id, dep, direc, pass)
        class(bst), intent(inout) :: this
        integer, intent(in) :: id
        character(len=*), intent(in) :: dep, direc, pass
        type(node_sucursal), pointer :: temp

        allocate(temp)
        temp%id_sucursal = id
        temp%departament = dep
        temp%dirrecion = direc
        temp%password = pass

        if(associated(this%root))then
            call add_rec(this%root, temp)
        else
            this%root => temp
        end if
    end subroutine add_bst

    recursive subroutine add_rec(current, new)
        type(node_sucursal), pointer, intent(inout) :: current
        type(node_sucursal), pointer, intent(in) :: new

        if(new%id_sucursal < current%id_sucursal)then
            
            if(associated(current%left))then
                call add_rec(current%left, new)
            else
                allocate(current%left)
                current%left => new
            end if

        else if(new%id_sucursal > current%id_sucursal)then
            if(associated(current%right))then
                call add_rec(current%right, new)
            else
                allocate(current%right)
                current%right => new
            end if
        end if

    end subroutine add_rec


    recursive subroutine inorderTraversal(this, current)
        class(bst), intent(in) :: this
        type(node_sucursal), pointer, intent(in) :: current
        
        if(.not. associated(current)) return
        call this%inorderTraversal(current%left)
        print *, 'Id sucursal', current%id_sucursal, 'Departamento: ', current%departament
        call this%inorderTraversal(current%right)

    end subroutine inorderTraversal

    ! Buscar una sucursal por id
    function search_node_bst(this, id, password) result(found_node)
        class(bst), intent(in) :: this
        integer, intent(in) :: id
        character(len=*), intent(in) :: password

        type(node_sucursal), pointer :: found_node
        found_node => null()
        if(associated(this%root))then
            found_node => search_node_bst_rec(this%root, id, password)
        end if
    end function search_node_bst

    ! buscar recursivamente en el arbol BST la sucursal por id
    recursive function search_node_bst_rec(node, id, password) result(found_node)
        type(node_sucursal), intent(in), pointer :: node
        integer, intent(in) :: id
        character(len=*), intent(in) :: password

        type(node_sucursal), pointer :: found_node

        found_node => null()
        if(associated(node))then
            if(node%id_sucursal == id .and. node%password == password)then
                found_node => node
            else if(id < node%id_sucursal)then
                found_node => search_node_bst_rec(node%left, id, password)
            else if(id > node%id_sucursal)then
                found_node => search_node_bst_rec(node%right, id, password)
            end if
        end if
    end function search_node_bst_rec 



 ! :::::::  Fucniones y subrutinas reutilizables ::::::: 

    function writeInteger(num) !function para convertir un entero a un String 
        integer, intent(in) :: num
        character(len=32) :: writeInteger
        write(writeInteger, '(I0)') num
        writeInteger = adjustl(trim(writeInteger))
    end function writeInteger

! :::::: Metodos para calculas SHA256 - AUTOR: Mikael Leetmaa 2014 
    function sha256(str)
        character(len=64) :: sha256
        character(len=*), intent(in) :: str
        sha256 = sha256b(str, 1)
    end function sha256

    function dirty_sha256(str)
        character(len=64) :: dirty_sha256
        character(len=*), intent(in) :: str
        dirty_sha256 = sha256b(str, 0)
    end function dirty_sha256

    function sha256b(str, swap)
        character(len=64) :: sha256b
        character(len=*), intent(in) :: str
        integer, intent(in) :: swap
        integer(kind=c_int64_t) :: length
        integer(kind=c_int32_t) :: temp1, temp2, i
        integer :: break, pos0
        integer(kind=c_int32_t) :: h0_ref(8), k0_ref(64)
        integer(kind=c_int32_t) :: h0(8), k0(64), a0(8), w0(64)
        data (h0_ref(i),i=1,8)/&
        & z'6a09e667', z'bb67ae85', z'3c6ef372', z'a54ff53a', z'510e527f', z'9b05688c', z'1f83d9ab', z'5be0cd19'/
        data (k0_ref(i), i=1,64)/&
        & z'428a2f98', z'71374491', z'b5c0fbcf', z'e9b5dba5', z'3956c25b', z'59f111f1', z'923f82a4', z'ab1c5ed5',&
        & z'd807aa98', z'12835b01', z'243185be', z'550c7dc3', z'72be5d74', z'80deb1fe', z'9bdc06a7', z'c19bf174',&
        & z'e49b69c1', z'efbe4786', z'0fc19dc6', z'240ca1cc', z'2de92c6f', z'4a7484aa', z'5cb0a9dc', z'76f988da',&
        & z'983e5152', z'a831c66d', z'b00327c8', z'bf597fc7', z'c6e00bf3', z'd5a79147', z'06ca6351', z'14292967',&
        & z'27b70a85', z'2e1b2138', z'4d2c6dfc', z'53380d13', z'650a7354', z'766a0abb', z'81c2c92e', z'92722c85',&
        & z'a2bfe8a1', z'a81a664b', z'c24b8b70', z'c76c51a3', z'd192e819', z'd6990624', z'f40e3585', z'106aa070',&
        & z'19a4c116', z'1e376c08', z'2748774c', z'34b0bcb5', z'391c0cb3', z'4ed8aa4a', z'5b9cca4f', z'682e6ff3',&
        & z'748f82ee', z'78a5636f', z'84c87814', z'8cc70208', z'90befffa', z'a4506ceb', z'bef9a3f7', z'c67178f2'/
        h0 = h0_ref
        k0 = k0_ref
        break = 0
        pos0 = 1
        length = len(trim(str))
        do while (break .ne. 1)
            call consume_chunk(str, length, w0(1:16), pos0, break, swap)
        do i=17,64
                    w0(i) = ms1(w0(i-2)) + w0(i-16) + ms0(w0(i-15)) + w0(i-7)
            end do
            a0 = h0
            do i=1,64
                    temp1 = a0(8) + cs1(a0(5)) + ch(a0(5),a0(6),a0(7)) + k0(i) + w0(i)
                    temp2 = cs0(a0(1)) + maj(a0(1),a0(2),a0(3))
                    a0(8) = a0(7)
                    a0(7) = a0(6)
                    a0(6) = a0(5)
                    a0(5) = a0(4) + temp1
                    a0(4) = a0(3)
                    a0(3) = a0(2)
                    a0(2) = a0(1)
                    a0(1) = temp1 + temp2
        end do
            h0 = h0 + a0
        end do
        write(sha256b,'(8z8.8)') h0(1), h0(2), h0(3), h0(4), h0(5), h0(6), h0(7), h0(8)
    end function sha256b


    function swap32(inp)
        integer(kind=c_int32_t) :: swap32
        integer(kind=c_int32_t), intent(in)  :: inp
        call mvbits(inp, 24, 8, swap32,  0)
        call mvbits(inp, 16, 8, swap32,  8)
        call mvbits(inp,  8, 8, swap32, 16)
        call mvbits(inp,  0, 8, swap32, 24)
    end function swap32

    function swap64(inp)
        integer(kind=c_int64_t) :: swap64
        integer(kind=c_int64_t), intent(in)  :: inp
        call mvbits(inp, 56, 8, swap64,  0)
        call mvbits(inp, 48, 8, swap64,  8)
        call mvbits(inp, 40, 8, swap64, 16)
        call mvbits(inp, 32, 8, swap64, 24)
        call mvbits(inp, 24, 8, swap64, 32)
        call mvbits(inp, 16, 8, swap64, 40)
        call mvbits(inp,  8, 8, swap64, 48)
        call mvbits(inp,  0, 8, swap64, 56)
    end function swap64

    function swap64a(inp)
        integer(kind=c_int64_t) :: swap64a
        integer(kind=c_int64_t), intent(in)  :: inp
        call mvbits(inp,  0, 8, swap64a, 32)
        call mvbits(inp,  8, 8, swap64a, 40)
        call mvbits(inp, 16, 8, swap64a, 48)
        call mvbits(inp, 24, 8, swap64a, 56)
        call mvbits(inp, 32, 8, swap64a,  0)
        call mvbits(inp, 40, 8, swap64a,  8)
        call mvbits(inp, 48, 8, swap64a, 16)
        call mvbits(inp, 56, 8, swap64a, 24)
    end function swap64a

    function ch(a, b, c)
        integer(kind=c_int32_t) :: ch
        integer(kind=c_int32_t), intent(in) :: a, b, c
        ch = ieor(iand(a, b), (iand(not(a), c)))
    end function ch

    function maj(a, b, c)
        integer(kind=c_int32_t) :: maj
        integer(kind=c_int32_t), intent(in) :: a, b, c
        maj = ieor(iand(a, b), ieor(iand(a, c), iand(b, c)))
    end function maj

    function cs0(a)
        integer(kind=c_int32_t) :: cs0
        integer(kind=c_int32_t), intent(in) :: a
        cs0 = ieor(ishftc(a, -2), ieor(ishftc(a, -13), ishftc(a, -22)))
    end function cs0

    function cs1(a)
        integer(kind=c_int32_t) :: cs1
        integer(kind=c_int32_t), intent(in) :: a
        cs1 = ieor(ishftc(a, -6), ieor(ishftc(a, -11), ishftc(a, -25)))
    end function cs1

    function ms0(a)
        integer(kind=c_int32_t) :: ms0
        integer(kind=c_int32_t), intent(in) :: a
        ms0 = ieor(ishftc(a, -7), ieor(ishftc(a, -18), ishft(a, -3)))
    end function ms0

    function ms1(a)
        integer(kind=c_int32_t) :: ms1
        integer(kind=c_int32_t), intent(in) :: a
        ms1 = ieor(ishftc(a, -17), ieor(ishftc(a, -19), ishft(a, -10)))
    end function ms1

    subroutine consume_chunk(str, length, inp, pos0, break, swap)
        character(len=*), intent(in) :: str
        integer(kind=c_int64_t), intent(in) :: length
        integer(kind=c_int32_t), intent(inout) :: inp(*)
        integer, intent(inout) :: pos0, break
        integer, intent(in) :: swap
        character(len=4) :: last_word
        integer(kind=c_int64_t) :: rest
        integer(kind=c_int32_t) :: to_pad, leftover, space_left, zero
        integer(kind=c_int8_t)  :: ipad0, ipad1, i
        data zero  / b'00000000000000000000000000000000'/
        data ipad0 / b'00000000' /
        data ipad1 / b'10000000' /
        rest = length - pos0 + 1
            if (rest .ge. 64) then
                inp(1:16) = transfer(str(pos0:pos0+64-1), inp(1:16))
                if (swap .eq. 1) then
                        do i=1,16
                            inp(i) = swap32(inp(i))
                        end do
                end if
                pos0 = pos0 + 64
                else
                    space_left = 16
                    leftover   = rest/4
                    if (leftover .gt. 0) then
                            inp(1:leftover) = transfer(str(pos0:pos0+leftover*4-1), inp(1:16))
                            if (swap .eq. 1) then
                                do i=1,leftover
                                    inp(i) = swap32(inp(i))
                                end do
                            end if
                            pos0 = pos0 + leftover*4
                            rest = length - pos0 + 1
                            space_left = space_left - leftover
                end if

                if (space_left .gt. 0) then
                        if (break .ne. 2) then
                            if (rest .gt. 0) then
                                last_word(1:rest) = str(pos0:pos0+rest-1)
                                pos0 = pos0 + rest
                            end if
                            last_word(rest+1:rest+1) = transfer(ipad1, last_word(1:1))
                            to_pad = 4 - rest - 1
                    do i=1, to_pad
                                last_word(rest+1+i:rest+1+i) = transfer(ipad0, last_word(1:1))
                            end do
                            inp(17-space_left) = transfer(last_word(1:4), inp(1))
                            if (swap .eq. 1) then
                                inp(17-space_left) = swap32(inp(17-space_left))
                            end if
                            space_left = space_left - 1
                            break = 2
                        end if
                        if (space_left .eq. 1) then
                            inp(16) = zero
                            space_left = 0
                        end if
                        rest = 0
                end if

                if ((rest .eq. 0) .and. (space_left .ge. 2)) then
                        do while (space_left .gt. 2)
                            inp(17-space_left) = zero
                            space_left = space_left - 1
                        end do
                        inp(15:16) = transfer(swap64a(length*8), inp(15:16))
                        break = 1
                end if
            end if
    end subroutine consume_chunk

end module pixel_print_studio

program main

    use json_module
    use pixel_print_studio
    
    implicit none

    !Variables para el menu principal
    integer :: opcion_menu

    ! VARIABLES PARA MENU SUCURSAL
    integer :: op_menu_suc, id_sucursal_input ! opcion menu sucursal
    character(len=50) :: password_sucursal

    ! VARIABLES PARA MENU REPORTES
    integer :: op_menu_rep

    !variables constantes para ingresar sesion administrador
    character(len=*), parameter :: user_admin = "EDD1S2024"
    character(len=*), parameter :: password_admin = "ProyectoFase3"
    character(len=20) :: username_input
    character(len=20) :: password_input

    !Json 
    type(Json_file) :: json
    type(json_core) :: jsonc


    !Sucursales
    type(bst) :: mytree_bst
    type(node_sucursal), pointer :: pointer_sucursal

    ! ARBOL MERKLE
    type(tree_merkle) :: merkle
    character(len=:), allocatable :: valor

    ! PUNTEROS Y VARIABLES PARA LEER LAS SUCURSALES
    type(json_value), pointer :: p_list_S, p_sucursal, p_atributos_s
    integer :: i, size_surcursal
    logical :: found_s
    ! variables para insertar_sucursal
    integer :: id_sucursal 
    character(:), allocatable :: departament, dirrecion, password 

    ! PUNTEROS Y VARIABLES PARA LEER LAS RUTAS
    type(json_value), pointer :: p_list_r, p_ruta, p_atributos_r
    integer j, size_rutas
    logical :: found_r
    integer :: id_s1, id_s2, distancia, impresoras_mant 

    ! PUNTEROS Y VARIABLES PARA LEER TECNICOS
    type(json_value), pointer :: p_list_t, p_tecnico, p_atributos_t
    integer w, size_tecnicos
    logical :: found_t

    ! variables para insertar_tecnicos
    integer(kind=8) :: dpi_tecnico
    character(:), allocatable :: dpi_str, nombre, apellido, genero, direccion
    integer :: telefono
    character(len=256) :: name_json_load

    !VARIABLES PARA CALCULAR LA RUTA MAS OPTIMA
    integer :: vertice_destino
    integer*8 :: tecnico_input
    logical :: exist_v_origen=.false. , exist_v_destino=.false. , existe_tech

    !USO SHA256
    character(len=256) :: message 

    !JSON BLOCK CHAIN
    type(json_core) :: json_data
    
    !INSTANCIA LISTA SIMPLE BLOCK CHAIN
    type(block_chain) :: my_block_chain

    !INSTANCIA DE LISTA COMPLETA TECNICOS
    type(all_techinical) :: my_all_techinical

    !INSTANCIA DE LISTA COMPLETA SUCURSALES
    type(list_sucursales) :: my_all_list_sucursales

    ! Tabla HASH
    integer :: m = 7 
    integer :: maxi = 70
    type(hash) :: h

    ! GRAFO :::
    type(grafo) :: g
    type(ruta), pointer :: all_routes
    nullify(all_routes)

    !Inicializar Tabla Hash
    !call h%init(m, maxi)
    !print *, 'Initial size table: ', m
    !print *, 'Maximum percentage: ', maxi
    call json_data%initialize() !Inicializar json para crear nuestro arhivo json 
  



    print *, ":::::::::: Pixel Print Studio :::::::::"
    print *, " ----- Ingrese sus credenciales -----"
    print *, "Ingrese el nombre de usuario administrador:"
    read(*, '(A)') username_input
    print *, "Ingrese la contrasena:"
    read(*, '(A)') password_input
    if(trim(username_input) == user_admin .and. trim(password_input) == password_admin)then
    
        print *, " - - USUARIO ADMINISTRADOR - -"
        ! :::: Menu principal
        do while(opcion_menu /= 5)
            print *, ":::::::::: Pixel Print Studio :::::::::"
            print *, "[1] Carga de archivo - Sucursal"
            print *, "[2] Carga de archivo - Rutas"
            print *, "[3] Menu Sucursales"
            print *, "[4] Reportes"
            print *, "[5] Salir"
            read *, opcion_menu
            select case(opcion_menu)
                case (1) ! Carga archivo sucursal
                    call json%load(filename= "sucursales_aux.json")
                    call json%info('', n_children= size_surcursal)
                    call json%get_core(jsonc)
                    call json%get('', p_list_S, found_s)
    
                    do i = 1, size_surcursal
                        call jsonc%get_child(p_list_S, i, p_sucursal, found= found_s)
                        !Inicializamos las variables
                        id_sucursal = 0
                        departament = ''
                        dirrecion = ''
                        password = ''
    
                        call jsonc%get_child(p_sucursal, 'id', p_atributos_s, found= found_s)
                        if (found_s) then
                            call jsonc%get(p_atributos_s, id_sucursal)
                        end if
    
                        call jsonc%get_child(p_sucursal, 'departamento', p_atributos_s, found= found_s)
                        if (found_s) then
                            call jsonc%get(p_atributos_s, departament)
                        end if
    
                        call jsonc%get_child(p_sucursal, 'direccion', p_atributos_s, found= found_s)
                        if (found_s) then
                            call jsonc%get(p_atributos_s, dirrecion)
                        end if
    
                        call jsonc%get_child(p_sucursal, 'password', p_atributos_s, found= found_s)
                        if (found_s) then
                            call jsonc%get(p_atributos_s, password)
                        end if
                        call mytree_bst%add_bst(id_sucursal, departament, dirrecion, password)
    
                        call my_all_list_sucursales%add_node_list_suc(id_sucursal, departament, dirrecion) !Insertar una sucursal en la lista de sucursales
                    end do
                    print*, ""
                    print *, ":. Se genero la carga masiva de sucursales correctamente .:"
                    print*, ""
    
                call mytree_bst%inorderTraversal(mytree_bst%root)
    
    
                call mytree_bst%write_bst_to_json()
    
                case (2) ! Carga archivo rutas
                    
                    ! Inicializar el grafo
                    call g%initiGrafo()
    
                    call json%load(filename= "grafo.json")
                    call json%get('grafo', p_list_r, found_r)
                    call json%info('grafo', n_children= size_rutas)
                    call json%get_core(jsonc)
    
                    do j=1, size_rutas
                        call jsonc%get_child(p_list_r, j, p_ruta, found= found_r)
                        id_s1 = 0
                        id_s2 = 0
                        distancia = 0
                        impresoras_mant = 0
    
                        call jsonc%get_child(p_ruta, 's1', p_atributos_r, found= found_r)
                        if(found_r)then
                            call jsonc%get(p_atributos_r, id_s1)
                        end if
    
                        call jsonc%get_child(p_ruta, 's2', p_atributos_r, found= found_r)
                        if(found_r)then
                            call jsonc%get(p_atributos_r, id_s2)
                        end if
    
                        call jsonc%get_child(p_ruta, 'distancia', p_atributos_r, found= found_r)
                        if(found_r)then
                            call jsonc%get(p_atributos_r, distancia)
                        end if
    
                        call jsonc%get_child(p_ruta, 'imp_mantenimiento', p_atributos_r, found= found_r)
                        if(found_r)then
                            call jsonc%get(p_atributos_r, impresoras_mant)
                        end if
                        call g%InsertaVertice(id_s1)
                        call g%InsertaVertice(id_s2)
    
                        call g%InsertArista(id_s1, id_s2, distancia, impresoras_mant)
                    end do
    
                    print*, ""
                    print*, ":. Se genero la carga masiva de rutas correctamente .:"
                    print*, ""
    
                    call g%GenerarGrafoDot() !Graficar el grafo
                case (3) ! Menu Sucursal
                    op_menu_suc = 0
                    print *, "Ingrese las credenciales de la sucursal"
                    print *, "Ingrese el ID de la sucursal"
                    read(*, '(I10)') id_sucursal_input
                    print *, "Ingrese la contrasena de la sucursal"
                    read(*, '(A)') password_sucursal
                    pointer_sucursal => mytree_bst%search_node_bst(id_sucursal_input, password_sucursal)
                    if(associated(pointer_sucursal)) then
                        do while(op_menu_suc /= 5)
                            print *, "Menu Sucursales"
                            print *, "[1] - Carga de tecnicos"
                            print *, "[2] - Ruta optima"
                            print *, "[3] - Informacion de un tecnico"
                            print *, "[4] - Listar tecnico"
                            print *, "[5] - Salir"
                            print *, "Ingrese una opcion"
                            read *, op_menu_suc
                            select case (op_menu_suc)
                                case(1) !carga de tecnicos
                                    print *, "Ingrese el nombre del archivo JSON a cargar: "
                                    read(*, '(A)') name_json_load
                                    call json%load(filename=trim(name_json_load))
                                    !call json%load(filename="tecnicos.json")
                                    
                                    call json%info('', n_children= size_tecnicos)
                                    call json%get_core(jsonc)
                                    call json%get('', p_list_t, found_t)
                                    
                                    !Inicializar tabla
                                    if(.not. allocated(pointer_sucursal%tecnicos))then
                                        allocate(pointer_sucursal%tecnicos)
                                        call pointer_sucursal%tecnicos%init(m, maxi) 
                                    end if
                                    
                                    do i = 1, size_tecnicos
                                        call jsonc%get_child(p_list_t, i, p_tecnico, found= found_t)
                                        dpi_str = ''
                                        nombre = ''
                                        apellido = ''
                                        genero = ''
                                        direccion = ''
                                        telefono = 0
    
                                        call jsonc%get_child(p_tecnico, 'dpi', p_atributos_t, found= found_t)
                                        if(found_t)then
                                            call jsonc%get(p_atributos_t, dpi_str)
                                            read(dpi_str, *) dpi_tecnico
                                        end if
    
                                        call jsonc%get_child(p_tecnico, 'nombre', p_atributos_t, found= found_t)
                                        if(found_t)then
                                            call jsonc%get(p_atributos_t, nombre)
                                        end if
    
                                        call jsonc%get_child(p_tecnico, 'apellido', p_atributos_t, found= found_t)
                                        if(found_t)then
                                            call jsonc%get(p_atributos_t, apellido)
                                        end if
    
                                        call jsonc%get_child(p_tecnico, 'genero', p_atributos_t, found= found_t)
                                        if(found_t)then
                                            call jsonc%get(p_atributos_t, genero)
                                        end if
                                        
                                        call jsonc%get_child(p_tecnico, 'direccion', p_atributos_t, found= found_t)
                                        if(found_t)then
                                            call jsonc%get(p_atributos_t, direccion)
                                        end if
    
                                        call jsonc%get_child(p_tecnico, 'telefono', p_atributos_t, found= found_t)
                                        if(found_t)then
                                            call jsonc%get(p_atributos_t, telefono)
                                        end if
                                        !AÑADIR TECNICOS A MI TABLA HASH
                                        call pointer_sucursal%tecnicos%insert(&
                                        techinical(dpi_tecnico, nombre, apellido, genero, direccion, telefono))
    
                                        !AÑADIR TECNICOS A MI LISTA TECNICOS GENERAL
                                        call my_all_techinical%add_techical(dpi_tecnico, nombre, apellido)
                                    end do
                                    
                                    call json%destroy()
                                    print *, ":. La carga masiva de tecnicos se realizo correctamente .:"
                                    call pointer_sucursal%tecnicos%graph_hash_table("table_has.dot")
                                case(2) !Ruta mas optimas 
                                    print*, "Ingrese el vertice destino"
                                    read(*, '(I10)') vertice_destino
                                    print*, "Ingrese el  tecnico que realizara el trabajo"
                                    read(*, '(I20)') tecnico_input
                                    
                                    exist_v_origen = BuscarVertice(g, pointer_sucursal%id_sucursal)
                                    exist_v_destino = BuscarVertice(g, vertice_destino)
                                    existe_tech = pointer_sucursal%tecnicos%find_technician(tecnico_input)
    
                                    !BUSCAR EN LA LISTA DE TECNICOS SI EXISTE EL DPI INCREMENTAR TRABAJOS REALIZADOS
                                    call my_all_techinical%find_one_techical(tecnico_input)
    
                                    
                                    if(exist_v_origen .and. exist_v_destino .and. existe_tech)then  ! Si existe (Vertice origen, Vertice destino, Tecnico)
                                        
                                        call g%optimal_route(pointer_sucursal%id_sucursal, vertice_destino, &
                                        tecnico_input, mytree_bst, json_data, my_block_chain, my_all_list_sucursales)
                                    
                                        !call my_block_chain%show_block_chain() !Mosntrar lista block chain
                                        call my_block_chain%grap_block_chain()
                                    end if
                                case(3) !Buscar tecnico por id
                                    print*, "Ingrese el DPI del tecnico"
                                    read(*, '(I20)') tecnico_input
                                    call pointer_sucursal%tecnicos%find_technician_id(tecnico_input)
                                case(4) !Listar todos los tecnicos
                                    call pointer_sucursal%tecnicos%list_all_technician()
    
                            end select
                        end do
    
                    else
                        print *, "No existe la sucursal con ese id"
                    end if
                case (4) !Reportes de la empresa
                    op_menu_rep = 0
                    do while(op_menu_rep /= 4)
                        print *, "--- Menu Reportes ---"
                        print *, "[1] - Reporte top 5 tecnicos con mas trabajos realizados"
                        print *, "[2] - Reporte top 5 sucursales con mas trabajos solicitados"
                        print *, "[3] - Reporte Ganancias y Costos totales de la empresa"
                        print *, "[4] - Salir"
                        print *, "Ingrese una opcion"
                        read *, op_menu_rep
                        select case (op_menu_rep)
                            case(1) !REPORTE TOP 5 TECNICOS CON MAS TRABAJOS REALIZADOS
                                !Ordenar top 5 tecnicos con mayor numero de trabajos realizados
                                call my_all_techinical%bublle_sort_tech()
                                !Listar los top 5 tecnicos con mayor numero de trabajos realizados 
                                call my_all_techinical%top_5_tech()
                            case (2)
                                !Ordenar top 5 Sucursales con mayor numero de trabajos solicitados
                                call my_all_list_sucursales%bublle_sort_list_suc()
                                !Listar las top 5 sucursales con mayor numero de trabajos solicitados
                                call my_all_list_sucursales%top_5_sucusales()
                            case (3)
                                write(*, '(A, I0)') "Ingresos totales de la empresa: + $ ", ingresos_extras_totales
                                write(*, '(A, I0)') "Costos totales de la empresa: - $ ", costos_totales
                                write(*, '(A, I0)') "Ganancias totales de la empresa:  ", ganancias_totales
    
                        end select
                    end do
    
            end select
        end do
        
    else
        print *, "Datos incorrectos para el administrador"
    end if



    message = "Este es un Hash256!"
    print *, "Este es un Hash256!"
    print*, sha256(message)

end program main