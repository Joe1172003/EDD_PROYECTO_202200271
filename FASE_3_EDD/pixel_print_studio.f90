module pixel_print_studio
    implicit none

    integer :: uid = 1

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


    type node_techinical
        integer*8 :: dpi
        character(len=32) :: name
        character(len=32) :: last_name
        integer :: jobs_done = 0
        type(node_techinical), pointer :: next_techincal
    end type node_techinical

    type all_techinical
        type(node_techinical), pointer :: head_techincal
        contains
            procedure :: add_tencial
    end type all_techinical

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
    end type

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
            procedure :: xor8
            procedure :: showhash
            procedure :: show_dataBlock
            procedure :: generate
            procedure :: dot_tree_merkle
            procedure :: grap_Tree_merkle
    end type tree_merkle


    


    contains

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
                    node%hash = this%xor8(hash)
                else
                    hash = adjustl(node%left%hash(1:len(node%left%hash)/2))// &
                    adjustl(node%right%hash(1:len(node%right%hash)/2))
                    node%hash = this%xor8(hash)
                end if
            end if
    end subroutine genhash

    function xor8(this, str) result(hash)
        class(tree_merkle), intent(inout) :: this
        character(len=:), allocatable, intent(in) :: str
        character(len=8), allocatable :: hash
        integer :: i, resultbyte
        character(len=2) :: hexbyte
        integer :: ascii_code
        hash = "00000000"
        do i=1, len(str)
            ascii_code = ichar(str(i:i))
            resultbyte = ieor(ascii_code, 8) 
            write(hexbyte, '(Z2)') resultbyte
            hash = trim(hexbyte)//trim(hash) 
        end do
    end function xor8

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
    function datablok(this, pos) result(left_node)
        class(tree_merkle), intent(inout) :: this
        integer, intent(inout) :: pos
        type(node_dataBloken), pointer :: left_node
        left_node => this%data_head
        do while(associated(left_node))
            if(pos == 0) return
            pos = pos - 1 
            left_node => left_node%next
        end do
    end function datablok

    subroutine grap_Tree_merkle(this)
        class(tree_merkle), intent(inout) :: this
        integer :: io, iostat, exitstat
        character(len=100) :: dot_command

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
        if(.not. associated(current)) return
        write(unit, '(A, I5, A, A, A)')' ', current%uid, '[label="', current%hash, '"];' !top hash
        
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
        vertex_dest => ObtenerVertice(this, dest_name)

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
                    '("    ", a, " -> ", a, " [color=green label=<", i0, "<br/><font color=''red''>", i0, "</font>>];")') &
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
            print *, "ganancias: ", ganancias
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
                    print *, "Costo: ", costos
                    print *, "Ganancias: ", ganancias
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
    
        paso => destino
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
    subroutine optimal_route(this, origen, destino, id_tecnico, bst_tree)
        class(grafo), intent(inout) :: this
        type(bst), intent(in) :: bst_tree
        type(ruta), pointer :: all_routes
        type(tree_merkle) :: merkle ! Al finalizar la ejecución de la subrutina, cualquier dato almacenado en esta instancia se perderá si no se ha guardado o retornado de alguna manera.
        integer, intent(in) :: origen, destino
        integer*8, intent(in) :: id_tecnico

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
        type(vertice), pointer :: current_vertex, next_vertex
        type(arista), pointer :: current_edge !actual arista 
        integer :: route_distance
        character(len=:), allocatable :: info_concatenada
        integer index
        
        nullify(all_routes) ! nulificamos nuestro puntero all_routes 

        ! llamamos a la funcion buscar mejor ruta por distancia
        call this%best_route(origen, destino, ruta_dis, distancia_rout1, totalImpresoras_rout1, successfully)
        
        call this%find_all_routes(origen, destino, all_routes)
        call best_maxRoute(all_routes, ruta_print, distancia_rout2, totalImpresoras_rout2 )
    
        if (successfully) then ! Si se encuentra la ruta minima
            total1 = (totalImpresoras_rout1 * costo_reparacion) - (distancia_rout1 * costo_distancia)
            total2 = (totalImpresoras_rout2 * costo_reparacion) - (distancia_rout2 * costo_distancia)
            
            print *, "--------------------------------------------------------" 
            print *, "total 1 - distancia Minima:", total1
            print *, "total 2 - Maximo numero de impresoras reparadas:", total2
            print *, "--------------------------------------------------------"

            !comparar la dos rutas - peso distancia - maxima cantida de impresoras reparadas
            if(total1 > total2)then
                call this%graph_best_route(ruta_dis)
                allocate(ruta_optimal(size(ruta_dis)))
                ruta_optimal = ruta_dis
            else
                call this%graph_best_route(ruta_print)
                allocate(ruta_optimal(size(ruta_print)))
                ruta_optimal = ruta_print
            end if

            !Obtener info para mi nodos hoja de mi arbol Merkle
            do index = 1, size(ruta_optimal) - 1

                sucursal_info => bst_tree%search_node_bst_id(ruta_optimal(index)) !obtener info sucursal primera     
                next_sucursal_info => bst_tree%search_node_bst_id(ruta_optimal(index + 1)) !obtener info sucursal segunda posicion

                current_vertex => find_vertex_by_id(this%first_vertex, ruta_optimal(index))
                next_vertex => find_vertex_by_id(this%first_vertex, ruta_optimal(index + 1))

                if(associated(current_vertex))then !Ecnontro vertice inial, vertice siguiente
                    current_edge => current_vertex%ari
                    do while(associated(current_edge))
                        if(current_edge%dest_vertex%vertex_id == ruta_optimal(index + 1))then
                            route_distance = current_edge%distance
                            exit
                        end if
                        current_edge => current_edge%next_ari
                    end do
                end if

                if(associated(sucursal_info) .and. associated(next_sucursal_info))then
                    info_concatenada = ""
                    info_concatenada = "ID Sucursal Origen: " // trim(adjustl(writeInteger(ruta_optimal(index)))) // "\n "// &
                    "Dirección: " // trim(sucursal_info%dirrecion) // "\n "// &
                    "ID Sucursal Destino: " // trim(adjustl(writeInteger(ruta_optimal(index + 1)))) // "\n "// &
                    "Dirección: " // trim(next_sucursal_info%dirrecion)// "\n "//& 
                    "Costo total: " // trim(adjustl(writeInteger(route_distance)))
                end if
                !print *, info_concatenada
                call merkle%add(info_concatenada)
            end do

            call merkle%generate()
            !call merkle%showhash(merkle%tophash)
            call merkle%grap_Tree_merkle()
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
    
        if(exitstat /= 0) then
            print *, "Error al crear la grafica"
        else
            print *, "Se creo correctamente la grafica"
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

    !Subrutinas y funciones para añadir a la Lista auxiliar tecnicos 
    subroutine add_tencial(this, dpi, name, last_name)
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
            do while(associated(current_tech))
                current_tech => current_tech%next_techincal
            end do
            current_tech%next_techincal => new_tech
        end if
    end subroutine add_tencial

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

  


   

    ! :::: Menu principal
    do while(opcion_menu /= 4)
        print *, ":::::::::: Pixel Print Studio :::::::::"
        print *, "[1] Carga de archivo - Sucursal"
        print *, "[2] Carga de archivo - Rutas"
        print *, "[3] Menu Sucursales"
        print *, "[4] Salir"
        read *, opcion_menu
        select case(opcion_menu)
            case (1) ! Carga archivo sucursal
                call json%load(filename= "sucursales.json")
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
                end do
            print*, ""
            print *, ":. Se genero la carga masiva de sucursales correctamente .:"
            print*, ""

            !call mytree_bst%inorderTraversal(mytree_bst%root)
            case (2) ! Carga archivo rutas
                
                ! Inicializar el grafo
                call g%initiGrafo()

                call json%load(filename= "rutas.json")
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
                                        print*, direccion
                                    end if

                                    call jsonc%get_child(p_tecnico, 'telefono', p_atributos_t, found= found_t)
                                    if(found_t)then
                                        call jsonc%get(p_atributos_t, telefono)
                                    end if
                                    call pointer_sucursal%tecnicos%insert(&
                                    techinical(dpi_tecnico, nombre, apellido, genero, direccion, telefono))
                                end do
                                
                                call json%destroy()
                                call pointer_sucursal%tecnicos%graph_hash_table("table_has.dot")
                            case(2) !Ruta mas optimas 
                                print*, "Ingrese el vertice destino"
                                read(*, '(I10)') vertice_destino
                                print*, "Ingrese el  tecnico que realizara el trabajo"
                                read(*, '(I20)') tecnico_input
                                
                                exist_v_origen = BuscarVertice(g, pointer_sucursal%id_sucursal)
                                exist_v_destino = BuscarVertice(g, vertice_destino)
                                existe_tech = pointer_sucursal%tecnicos%find_technician(tecnico_input)

                                if(exist_v_origen .and. exist_v_destino .and. existe_tech)then  ! Si existe (Vertice origen, Vertice destino, Tecnico)
                                    call g%optimal_route(pointer_sucursal%id_sucursal, vertice_destino, tecnico_input, mytree_bst)
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
        end select
    end do

    !call mytree_bst%inorderTraversal(mytree_bst%root)


    ! Mostrar la lista de adyacencia del grafo
    !call g%MostrarListaAdyacencia()
    !call g%GenerarGrafoDot()


    !valor = "data1"
    !call merkle%add(valor)
    !valor = "data2"
	!call merkle%add(valor)
	!valor = "data3"
    !call merkle%add(valor)
    !call merkle%generate()
    !call merkle%showhash(merkle%tophash)

    !llamada 1 
    !call g%best_route(1,2)

    !call g%best_route_maxPrintMant(3,6)
    
    !call g%find_all_routes(1,9, all_routes)
    !call best_maxRoute(all_routes)
    !call show_all_routes(all_routes)

    !call g%find_all_routes(1,6, all_routes)
    !call best_maxRoute(all_routes)
    !call show_all_routes(all_routes)

    !llamada 2 
    !call g%best_route(6, 1)

    !print *, "Grafo"
    !call g%MostrarListaAdyacencia()


end program main