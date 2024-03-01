module Fase_1

    implicit none
      ! contadores globales asignados aqui 
      integer :: global_img_id = 0 ! cada id_imagen sera unica 
      integer :: contador_ejecutar = 0 ! control por cada llamada de mi funcion ejecutar
      integer :: contador_global = 0
    !-------------  Cliente 
    type :: Client
        integer :: id
        character(len=50) :: name
        integer :: img_p
        integer :: img_g
        integer :: copy_img_p , copy_img_g !copia de imagenes pequeñas y grandes
    end type 
  
    type :: NodoClient
        type(Client) :: data
        type(NodoClient), pointer :: next => null()
    end type 
  
    !----- Cola Repecion
    type :: Tail 
        type(NodoClient), pointer :: head => null()
        type(NodoClient), pointer :: tail => null()
        contains
        procedure :: encolar
        procedure :: show
        procedure :: desencolar
    end type
    !------------- end Cliente
  
  
    !--- Cola de Impresion Pequeña
    type :: ImpresoraP
      integer :: id_cliente
      character(len = 1) :: tipo_imagen
    end type
  
    type :: NodoImpresionP
      type(ImpresoraP) :: impresoraP_data
      type(NodoImpresionP), pointer :: next => null()
    end type 
  
    type :: Cola_Impresion_P
      type(NodoImpresionP), pointer :: head => null()
      type(NodoImpresionP), pointer :: tail => null()
      contains 
      procedure :: encolar_imp_p
      procedure :: show_imp_p
   end type 
  
  
   !--- Cola de Impresion Pequeña
  type :: ImpresoraG
      integer :: id_cliente
      character(len = 1) :: tipo_imagen
  end type
  
  type :: NodoImpresionG
      type(ImpresoraG) :: impresoraG_data
      type(NodoImpresionG), pointer :: next => null()
  end type 
  
  type :: Cola_Impresion_G
      type(NodoImpresionG), pointer :: head => null()
      type(NodoImpresionG), pointer :: tail => null()
      contains 
      procedure :: encolar_imp_g
      procedure :: show_imp_g
  end type 
  
  
  !------ Pila de imagenes
      ! Objeto imagen
      type :: Imagen
      integer :: id_img
      character(len = 1) :: type_img ! tipo imagen (P (pequeña) & G (Grande))
      integer :: id_client  ! id_client id que pertenece esa imagen
      end type 
  
      type :: NodoImagen
      type(Imagen) :: data_img
      type(NodoImagen), pointer :: next => null()
      end type
  
      type :: PilaImagenes
      type(NodoImagen), pointer :: top => null()
      integer :: contador = 0
      contains
      procedure :: push_img
      procedure :: show_img
      procedure :: clear_stack
      end type
  !------ end Pila de imagenes
  
    !-------------- Ventanilla
    type :: Ventanilla
        integer :: id
        integer :: id_cliente_asociado = 0
        integer :: paso = 0    
        type(Client), pointer :: assing_client => null()
        type(PilaImagenes) :: pila_imgs
    end type 
  
    type :: NodoVentanilla
        type(Ventanilla) :: dataV
        type(NodoVentanilla), pointer :: next => null()
    end type
  
    type :: VentanillaList
        type(NodoVentanilla), pointer :: head => null()
        contains 
        procedure :: add_ventanilla
        procedure :: show_ventanilla
        !Asignar un cliente a Ventanilla
        !procedure :: assing_client
    end type
  
    !------------end Ventanilla
  
    ! ---------- Lista de Listas  Clientes Espera AQUI COMIENZA ESTRUCTURA
      
      !----- Objeto sublista Imagen asociada al Cliente Espera
      type ::  img_asociada
          integer :: id_cliente_a
          character (len = 1) :: tipo_img_a
      end type
  
      ! sub Nodo para img_asociada
      type :: sub_nodo 
          type(img_asociada) :: img_data_a ! contiene los datos de el objeto img_asociada
          type(sub_nodo), pointer :: next => null()
      end type
  
      type :: cliente_espera
          integer :: id_cliente_e
          character (len = 200) :: nombre_cliente
          integer :: img_g_e
          integer :: img_p_e
          integer :: img_g_copy = 0  ! sumar imagen grande cada vez que se inserta una grande
          integer :: img_p_copy = 0 ! sumar imagen pequeña vez que se inserta una pequeña
          integer :: id_ventanilla
          integer :: paso = 0
      end type
  
      ! Nodo cliente en espera
      type :: nodo_cliente_e
          type(cliente_espera) :: cliente_data_e
          type(nodo_cliente_e), pointer :: next => null() 
          type(nodo_cliente_e), pointer :: prev => null()
          type(sub_nodo), pointer :: list_img_a => null()
      end type
  
      ! Lista de Lista cliente espera
      type :: lista_lista_cliente_espera
          type(nodo_cliente_e), pointer :: head => null()
          type(nodo_cliente_e), pointer :: tail => null()
      contains 
          procedure :: add_cliente_espera
          procedure :: show_cliente_espera
          procedure :: add_img_a_cliente
          procedure :: delete_cliente_en_espera
      end type
      
  
    ! ---------- end Lista de Listas Clientes de espera AQUI FINALIZA ESTRUCTURA
  
    ! ----- Lista de clientes atendidos 

      type :: cliente_finalizado
        integer :: id_cliente
        integer :: id_ventanilla
        character (len = 200) :: nombre_cliente
        integer :: img_p_recividas
        integer :: img_g_recividas
        integer :: total_pasos
      end type


      type :: Nodo_finalizado
        type(cliente_finalizado) :: data_finalizado
        type(Nodo_finalizado), pointer :: next => null()
      end type

      type :: list_finalizados
        type(Nodo_finalizado), pointer :: head => null()
        contains 
        procedure :: add_list_f
        procedure :: order_img_G
        !procedure :: order_img_P
      end type
    ! ------ end Lista de clientes atendidos


      contains   
      !agregar a cola reception
      subroutine encolar(this, cliente)
          class(Tail), intent(inout) :: this
          type(Client), intent(in) :: cliente
          type(NodoClient), pointer :: tempClient
          allocate(tempClient)
          tempClient%data = cliente
          tempClient%next => null()
  
          if(associated(this%head)) then
              this%tail%next => tempClient
              this%tail => tempClient
          else
              this%head => tempClient
              this%tail => tempClient
          end if
      end subroutine encolar
  
      ! eliminar o sacar cola reception        
      subroutine desencolar(this, clienteDesencolado)
          class(Tail), intent(inout) :: this
          type(Client), pointer, intent(out) :: clienteDesencolado !--- Agurdamos el cliente Desencolado
          type(NodoClient), pointer :: tempClient
          
          clienteDesencolado => null()
          
          if(associated(this%head)) then
              allocate(clienteDesencolado)
              tempClient => this%head
              
              clienteDesencolado%id = tempClient%data%id
              clienteDesencolado%name = tempClient%data%name
              clienteDesencolado%img_g = tempClient%data%img_g
              clienteDesencolado%img_p = tempClient%data%img_p
              clienteDesencolado%copy_img_g = tempClient%data%copy_img_g
              clienteDesencolado%copy_img_p = tempClient%data%copy_img_p
  
              this%head => this%head%next
              if(.not. associated(this%head))then
              this%tail => null()
              end if
              deallocate(tempClient)
  
          else
              print *, "La cola esta vacia"
          end if
      end subroutine desencolar
      
      ! Mostrar Clientes en cola recepcion
      subroutine show(this)
          class(Tail), intent(in) :: this
          type(NodoClient), pointer :: nodo_actual
          nodo_actual => this%head
          do
              if (.not. associated(nodo_actual)) then
                  print *, "La cola esta vacia"
                  exit
              end if 
              print *, "id_cliente: ", nodo_actual%data%id
              print *, "nombre ", nodo_actual%data%name
              print *, "img-g ", nodo_actual%data%img_g
              print *, "img-p", nodo_actual%data%img_p
              print *, "copy-img-g", nodo_actual%data%copy_img_g
              print *, "copy-img-p", nodo_actual%data%copy_img_p
              print *, "----------------------------------"
              nodo_actual => nodo_actual%next
          end do
      end subroutine show  
      
      !Grafica cola recepcion
      subroutine grafica_cola_recepcion(this, filename)
            type(Tail), intent(inout) :: this
            character(len = *), intent(in) :: filename
            
            integer :: unit, count
            type(NodoClient), pointer :: actual
            character(len =200) :: nodeLabel
            character(len=32) :: idStr, imgPStr, imgGStr, tmpStr   ! Asumiendo que los números caben en 32 caracteres

            count = 0
        
            open(unit, file=filename, status= 'replace')
            write(unit, *) 'digraph Cola {'
            write(unit, *) 'rankdir=LR;' !Ordenamos los nodose de izquierda a derecha
            write(unit, *) 'node [shape=box, style=filled, color=blue, fillcolor=pink]'
            ! Escribir nodos y conexiones
            actual => this%head
            do while(associated(actual))
                ! Convertir el entero 'count' y otros enteros a cadena
                ! Convertir cada entero a cadena
                write(imgPStr, '(i0)') actual%data%img_p
                write(imgGStr, '(i0)') actual%data%img_g
                write(tmpStr, '(i0)') count
                ! Formar la etiqueta del nodo
                nodeLabel = '"node' // trim(tmpStr) //'"'// ' [label="Name: ' // trim(actual%data%name) // &
                            '\n '//'img_p: ' // trim(imgPStr) // &
                            '\n'//'img_g: ' // trim(imgGStr) // '"]'

                write(unit, *) nodeLabel
                
                ! conexion al siguiente nodo
                if(associated(actual%next)) then
                    write(unit, "('node',i0,' -> node',i0)") count, count+1
                end if
                actual => actual%next
                count = count + 1
            end do
            write(unit, *) '}'
            close(unit)

            ! Generar el archivo PNG utilizando Graphviz
            call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')

      end subroutine grafica_cola_recepcion

      ! agregar a cola impresion grande
      subroutine encolar_imp_g(this, data_impresora_g)
          class(Cola_Impresion_G), intent(inout) :: this
          type(ImpresoraG), intent(in) :: data_impresora_g
          type(NodoImpresionG), pointer :: temp_print_g
  
          allocate(temp_print_g)
          temp_print_g%impresoraG_data = data_impresora_g
          temp_print_g%next => null()
  
          if(associated(this%head))then
              this%tail%next => temp_print_g
              this%tail => temp_print_g
          else 
              this%head => temp_print_g
              this%tail => temp_print_g
          end if
      end subroutine encolar_imp_g
  
      ! agregar a cola impresion pequeña
      subroutine encolar_imp_p(this, data_impresora_p)
          class(Cola_Impresion_P), intent(inout) :: this
          type(ImpresoraP), intent(in) :: data_impresora_p
          type(NodoImpresionP), pointer :: temp_print_p
  
          allocate(temp_print_p)
          temp_print_p%impresoraP_data = data_impresora_p
          temp_print_p%next => null()
  
          if(associated(this%head))then
              this%tail%next => temp_print_p
              this%tail => temp_print_p
          else 
              this%head => temp_print_p
              this%tail => temp_print_p
          end if
      end subroutine encolar_imp_p
  
      ! mostrar cola de impresion pequeña 
      subroutine show_imp_p(this)
          class(Cola_Impresion_P), intent(inout) :: this
          type(NodoImpresionP), pointer :: temp_imp_p
          temp_imp_p => this%head
          do 
              if (.not. associated(temp_imp_p))then
                  print *, "La cola de impresora pequena esta vacia"
                  exit
              end if
              print *, "------------- COLA DE IMPRESION P ----------"
              print *, "id_cliente ",temp_imp_p%impresoraP_data%id_cliente
              print *, "tipo_imagen           " ,temp_imp_p%impresoraP_data%tipo_imagen
              print *, "--------------------------------------------"
  
              temp_imp_p => temp_imp_p%next
          end do
      end subroutine show_imp_p
  
       ! mostrar cola de impresion grande
      subroutine show_imp_g(this)
          class(Cola_Impresion_G), intent(inout) :: this
          type(NodoImpresionG), pointer :: temp_imp_g
          temp_imp_g => this%head
          do 
              if (.not. associated(temp_imp_g))then
                  print *, "La cola de impresora pequena esta vacia"
                  exit
              end if
              print *, "------------- COLA DE IMPRESION G ----------"
              print *, "id_cliente ", temp_imp_g%impresoraG_data%id_cliente
              print *, "tipo_imagen           ", temp_imp_g%impresoraG_data%tipo_imagen
              print *, "--------------------------------------------"
  
              temp_imp_g => temp_imp_g%next
          end do
      end subroutine show_imp_g
          
      !-- Subrutinas Ventanillas (add, assing, show)
      subroutine add_ventanilla(this, id)
          class(VentanillaList), intent(inout) :: this
          integer, intent(in) :: id
          type(NodoVentanilla), pointer :: tempV => null()
  
          allocate(tempV)
          tempV%dataV%id = id
          tempV%dataV%assing_client => null()
          tempV%next => this%head
          this%head => tempV
      end subroutine add_ventanilla
  
      !-- agregar una imagen a la pila 
      subroutine push_img(this, image)
          class(PilaImagenes), intent(inout) :: this
          type(Imagen), intent(in) :: image
          type(NodoImagen), pointer :: newImg 
  
          allocate(newImg)
          newImg%data_img = image
          newImg%next => this%top
          this%top => newImg
          this%contador = this%contador + 1
      end subroutine push_img
  
      ! -- Limpiamos nuestra pila hasta que quede vacia
      subroutine clear_stack(this)
          class(PilaImagenes), intent(inout) :: this
          type(NodoImagen), pointer :: tempNodo
  
          do while(associated(this%top))
              tempNodo => this%top
              this%top => this%top%next
              deallocate(tempNodo)
          end do 
     
      end subroutine clear_stack
  
      ! Asingar Cliente a ventanilla
      subroutine asingar_cliente(this, colaClientes)
  
          type(VentanillaList), intent(inout) :: this
          type(NodoVentanilla), pointer :: tempVentanilla
          type(Tail), intent(inout) :: colaClientes
          type(Client), pointer :: cliente_to_ventanilla
          character(len=100) :: mensaje

          tempVentanilla => this%head
          do while(associated(tempVentanilla))
              if(tempVentanilla%dataV%id_cliente_asociado == 0) then
                  call colaClientes%desencolar(cliente_to_ventanilla)
                  if(associated(cliente_to_ventanilla)) then
                      tempVentanilla%dataV%assing_client => cliente_to_ventanilla
                      tempVentanilla%dataV%id_cliente_asociado = cliente_to_ventanilla%id
                      tempVentanilla%dataV%paso = tempVentanilla%dataV%paso + 1

                      write(mensaje, '(">> EL CLIENTE ", I0, " INGRESA A LA VENTANILLA ", I0)') &   ! Mensaje
                      cliente_to_ventanilla%id, tempVentanilla%dataV%id
                      print *, mensaje
                      exit    
                  else
                      print *, '---- No hay mas clientes en la cola ---'
                      exit 
                  end if
              end if
              tempVentanilla => tempVentanilla%next !valla avanzando
          end do
      end subroutine asingar_cliente
  
      !graficar lista ventanilla y pila de clientes
      subroutine graficar_ventanillas(this, filename)
        type(VentanillaList), intent(in) :: this
        type(NodoVentanilla), pointer :: tempV
        type(NodoImagen), pointer :: tempI
        
        integer :: unit
        character(len=200) :: idStr, clientIdStr, imgIdStr, nodeLabel, edgeLabel
        character(len=*), intent(in) :: filename
        
        open(newunit=unit, file=filename, status='replace')
        write(unit, *) 'digraph VentanillaList {'
        write(unit, *) 'rankdir=TB;'
        write(unit, *) 'node [shape=record, style=filled, fillcolor=lightblue]'
        
        tempV => this%head
    
        ! Recorrer los nodos de la lista de ventanillas
        do while(associated(tempV)) 
            ! Crear un nodo para cada ventanilla
            write(idStr, '(i0)') tempV%dataV%id
            nodeLabel = 'Ventanilla' // trim(idStr) // ' [label="Ventanilla ID: ' // trim(idStr) // '"]'
            write(unit, *) nodeLabel

            tempI => tempV%dataV%pila_imgs%top
            if(associated(tempI)) then
                write(unit, *) 'subgraph cluster', trim(idStr),' {'
                write(unit, *) 'rankdir=TB;'
                write(unit, *) 'label= "Ventanilla ' // trim(idStr) //'"'
            end if

            do while(associated(tempI))
                ! Convertir enteros a cadena img_id, cliente_id
                write(imgIdStr, '(i0)') tempI%data_img%id_img
                write(clientIdStr, '(i0)') tempI%data_img%id_client
                    
                ! Formar la etiqueta del nodo imagen
                nodeLabel = 'node' // trim(imgIdStr) // ' [label="Img : ' // &
                            trim(imgIdStr) // '\n id cliente: ' // trim(clientIdStr) // &
                            '\nTipo: ' // trim(tempI%data_img%type_img) // '"]'
                write(unit, *) nodeLabel
                
                ! Conectar las imágenes en la pila si hay más de una
                if(associated(tempI%next)) then
                    write(edgeLabel, '(a, i0, a, i0)') 'node', tempI%data_img%id_img, ' -> node', tempI%next%data_img%id_img
                    write(unit, *) edgeLabel
                end if

                tempI => tempI%next
            end do
            
            if(associated(tempV%dataV%pila_imgs%top)) then
                write(unit, *) '}'
            end if    
                    
            ! Conectar la ventanilla actual con la siguiente, si existe
            if(associated(tempV%next)) then
                write(edgeLabel, '(a, i0, a, i0)') 'Ventanilla', tempV%dataV%id, ' -> Ventanilla', tempV%next%dataV%id
                write(unit, *) edgeLabel
            end if
    
            tempV => tempV%next
        end do
    
        write(unit, *) '}'
        close(unit)

         ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
        
      end subroutine graficar_ventanillas
    
      ! Asignar Imagen a la pila 
      subroutine asignar_imagen(this, cliente)
          type(VentanillaList), intent(inout) :: this
          type(NodoVentanilla), pointer :: tempVentanilla
          type(Client), intent(inout) :: cliente
          type(Imagen) :: nueva_imagen
          logical :: imagen_asignada
          character (len = 100) :: mensaje
  
          tempVentanilla => this%head
          do while(associated(tempVentanilla))
              if(tempVentanilla%dataV%id_cliente_asociado == cliente%id)then
  
                  if(cliente%img_p > 0)then   
                      cliente%img_p = cliente%img_p - 1
                      nueva_imagen%type_img = 'P'
                  else if(cliente%img_g > 0)then
                      cliente%img_g = cliente%img_g - 1
                      nueva_imagen%type_img = 'G'
                  else
                      imagen_asignada = .false.
                      !print *, "ESTAS AQUI :l"
                      exit  ! A mira ya no tenemos mas imagenes para asiganar asi que salite  !AQUI ESTABA COMO return
                  end if
                  global_img_id = global_img_id + 1
                  nueva_imagen%id_img = global_img_id
                  nueva_imagen%id_client = cliente%id
                  tempVentanilla%dataV%paso = tempVentanilla%dataV%paso + 1

                  !MENSAJE
                  write(mensaje, '(">> LA VENTANILLA ", I0, " RECIBIO UNA IMAGEN """, A, """ DEL CLIENTE ", I0)') &
                  tempVentanilla%dataV%id, nueva_imagen%type_img, tempVentanilla%dataV%id_cliente_asociado
                  print *, mensaje

                  call tempVentanilla%dataV%pila_imgs%push_img(nueva_imagen)
                  imagen_asignada = .true.
                  return
              end if
              tempVentanilla => tempVentanilla%next
          end do
      end subroutine asignar_imagen   
  
  
      !Añadir cliente es lista doblemente circula
      subroutine add_cliente_espera(this, cliente_e)
          class(lista_lista_cliente_espera), intent(inout) :: this
          type(cliente_espera), intent(in) :: cliente_e
          type(nodo_cliente_e), pointer :: nuevo
  
          allocate(nuevo)
          nuevo%cliente_data_e = cliente_e
  
          nuevo%next => null()
          nuevo%prev => null()
          
          if(.not. associated(this%head)) then
              this%head => nuevo
              this%tail => nuevo
              nuevo%next => nuevo
              nuevo%prev => nuevo
          else
              nuevo%next => this%head
              nuevo%prev => this%tail ! nuevo nodo apunte al anterior de la lista
              this%head%prev => nuevo  ! el primer nodo de la lista apunta hacia atras
              this%tail%next => nuevo ! aqui el nodo que se inserto al princio apunte al nuevo nodo que se ingreso
              this%tail => nuevo
  
          end if
      end subroutine add_cliente_espera
      
      !Asociar una imagen a un cliente
      subroutine add_img_a_cliente(this, id_cliente_asociado)
          class(lista_lista_cliente_espera), intent(inout) :: this
          integer, intent(in) :: id_cliente_asociado
  
          type(nodo_cliente_e), pointer :: aux
          type(sub_nodo), pointer :: nuevo, last_img
  
          aux => this%head
          ! Buscamos el cliente con el id_cliente_e  que sea igual al id cliente imagen
          do while(associated(aux))
              if(aux%cliente_data_e%id_cliente_e == id_cliente_asociado)then
                  allocate(nuevo)
                  nuevo%img_data_a%id_cliente_a = id_cliente_asociado
                  nuevo%next  => null()
  
                  !Anadimos la imagen a la lista de imagenes del cliente asociado por el id
                  if(.not. associated(aux%list_img_a))then
                      aux%list_img_a => nuevo
                  else
                      last_img => aux%list_img_a
                      do while(associated(last_img%next))
                          last_img => last_img%next
                      end do
                      last_img%next => nuevo
                  end if
                  return 
              end if
              aux => aux%next
          end do
      end subroutine add_img_a_cliente
  
      !Monstrar imagen de cliente en espera 
      subroutine show_cliente_espera(self)
          class(lista_lista_cliente_espera), intent(in) :: self
          type(nodo_cliente_e), pointer :: nodo_cliente_actual
          type(sub_nodo), pointer :: nodo_img_actual
  
          if (.not. associated(self%head))then
              print *, "-------------------"
              print *, "La lista esta vacia."
              print *, "-------------------"
  
              return
          end if
          nodo_cliente_actual => self%head
  
          do while(associated(nodo_cliente_actual))
              ! Mostrar información del cliente
              print *, "-----------------------------------"
              print *, "id cliente:", nodo_cliente_actual%cliente_data_e%id_cliente_e
              print *, "nombre:    ", nodo_cliente_actual%cliente_data_e%nombre_cliente

              print *, "ventanilla:", nodo_cliente_actual%cliente_data_e%id_ventanilla
              
              print *, "Imagenes grandes:", nodo_cliente_actual%cliente_data_e%img_g_e
              print *, "Imagenes pequenas:", nodo_cliente_actual%cliente_data_e%img_p_e

              print *, "Imagenes grandes Copy:", nodo_cliente_actual%cliente_data_e%img_g_copy
              print *, "Imagenes pequenas Copy:", nodo_cliente_actual%cliente_data_e%img_p_copy

              print *, "Paso: ", nodo_cliente_actual%cliente_data_e%paso
              ! Mostrar imagenes asociadas al cliente
              if (associated(nodo_cliente_actual%list_img_a)) then
                  print *, "Imagenes asociadas:"
                  nodo_img_actual => nodo_cliente_actual%list_img_a
                  do while(associated(nodo_img_actual))
                      print *, " - ID Imagen:", nodo_img_actual%img_data_a%id_cliente_a
                      print *,  "Tipo:", nodo_img_actual%img_data_a%tipo_img_a 
                      nodo_img_actual => nodo_img_actual%next
                  end do
              else
                  print *, "No hay imagenes asociadas."
  
              end if
              print *, "-----------------------------------"
  
              if(associated(nodo_cliente_actual%next, self%head)) exit ! salir al completar un ciclo (lista circular)
              ! Mover al siguiente cliente en la lista
              nodo_cliente_actual => nodo_cliente_actual%next
          end do
      end subroutine
  
      !Analizar ventanilla
      subroutine analizar_ventanilla(this, listaClientesEspera, colaImpresionP, colaImpresionG)
          type(VentanillaList), intent(inout) :: this
          type(NodoVentanilla), pointer :: tempVentanilla
  
          type(lista_lista_cliente_espera), intent(inout) :: listaClientesEspera
          type(cliente_espera) :: clienteTemp
  
          type(Cola_Impresion_P), intent(inout) :: colaImpresionP
          type(Cola_Impresion_G), intent(inout) :: colaImpresionG
  
          type(ImpresoraP) :: data_impresora_p
          type(ImpresoraG) :: data_impresora_g
  
          type(NodoImagen), pointer :: tempNodoImagen ! temporal nodo imagen

          !MENSAJE
          character(len = 200) :: mensaje1, mensaje2, mensajeCompleto
  
          tempVentanilla => this%head
          do while(associated(tempVentanilla))
              if(associated(tempVentanilla%dataV%assing_client))then
                  if((tempVentanilla%dataV%assing_client%img_p == 0 ) .and. (tempVentanilla%dataV%assing_client%img_g == 0)) then

                      tempNodoImagen => tempVentanilla%dataV%pila_imgs%top ! revisar si hay imagenes paara procesar
                      do while(associated(tempNodoImagen)) 
                          select case (tempNodoImagen%data_img%type_img) 
                          Case ('P')
                              data_impresora_p%id_cliente = tempNodoImagen%data_img%id_client
                              data_impresora_p%tipo_imagen = tempNodoImagen%data_img%type_img
                              call colaImpresionP%encolar_imp_p(data_impresora_p)
                          Case ('G')
                              data_impresora_g%id_cliente = tempNodoImagen%data_img%id_client
                              data_impresora_g%tipo_imagen = tempNodoImagen%data_img%type_img
                              call colaImpresionG%encolar_imp_g(data_impresora_g)
                          end select
                          tempNodoImagen => tempNodoImagen%next
                      end do
                      !!!!!!!nombre_cliente
                      ! creamos nuestra lista de clientes de espeta 
                      clienteTemp%id_cliente_e = tempVentanilla%dataV%assing_client%id
                      clienteTemp%nombre_cliente = tempVentanilla%dataV%assing_client%name
                      clienteTemp%img_g_e = tempVentanilla%dataV%assing_client%copy_img_g
                      clienteTemp%img_p_e = tempVentanilla%dataV%assing_client%copy_img_p
                      clienteTemp%id_ventanilla = tempVentanilla%dataV%id  ! <-------------------------------------------------[1]
                      clienteTemp%paso = tempVentanilla%dataV%paso + 1

                      !MENSAJE
                      write(mensaje1, '(">> EL CLIENTE ", I0, " ES ATENDIDO Y INGRESA A LA LISTA DE ESPERA")') &
                      tempVentanilla%dataV%assing_client%id

                      write(mensaje2, '("LA VENTANILLA ", I0,' // &
                      '" ENVIA LAS IMAGENES DEL CLIENTE ", I0, " A SUS RESPECTIVAS COLAS DE IMPRESION")') &
                      tempVentanilla%dataV%id, tempVentanilla%dataV%assing_client%id
                      
                      mensajeCompleto = trim(mensaje1) // "  ---  " // trim(mensaje2)
                      print *, mensajeCompleto
                       
                      call listaClientesEspera%add_cliente_espera(clienteTemp)
                      
                      !Limpiar la pila de imágenes de la ventanilla actual 
                      call tempVentanilla%dataV%pila_imgs%clear_stack()
  
                      ! Limpiar mi pila de imagenes AQUI
                      tempVentanilla%dataV%paso = 0
                      tempVentanilla%dataV%id_cliente_asociado = 0
                      nullify(tempVentanilla%dataV%assing_client)
                      
                  end if
              end if
              tempVentanilla => tempVentanilla%next
          end do  
      end subroutine analizar_ventanilla
  
      !Grafica de impresion de cola Pequeña
      subroutine grafica_impresionP(this, filename)
        type(Cola_Impresion_P), intent(inout) :: this
        character (len = *), intent(in) :: filename

        integer :: unit, count
        type(NodoImpresionP), pointer :: current
        character(len = 200) :: nodelabel, idStr, countStr

        count = 0
        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph ColaIP {'
        write(unit, *) 'rankdir=LR;' 
        write(unit, *) 'node [shape=box, style=filled, color=black, fillcolor=green]'
        current => this%head

        do while(associated(current))
            write(idStr, '(i0)') current%impresoraP_data%id_cliente
            write(countStr, '(i0)') count
            nodelabel = '"node' // trim(countStr) //'"'//' [label="Id_Cliente: ' // trim(idStr) // &
                        '\n '// trim(current%impresoraP_data%tipo_imagen) // '"]'
            
            write(unit, *) nodeLabel
            if (associated(current%next)) then 
                write(unit, "('node',i0,' -> node',i0)") count, count+1
            end if
            current => current%next
            count = count + 1
        end do
        write(unit, *) '}'
        close(unit)

        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')

      end subroutine grafica_impresionP

      !Grafica de impresion de cola Grande
    subroutine grafica_impresionG(this, filename)
        type(Cola_Impresion_G), intent(inout) :: this
        character (len = *), intent(in) :: filename

        integer :: unit, count
        type(NodoImpresionG), pointer :: current
        character(len = 200) :: nodelabel, idStr, countStr

        count = 0
        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph ColaIG {'
        write(unit, *) 'rankdir=LR;' 
        write(unit, *) 'node [shape=box, style=filled, color=black, fillcolor=blue]'
        current => this%head

        do while(associated(current))
            write(idStr, '(i0)') current%impresoraG_data%id_cliente
            write(countStr, '(i0)') count
            nodelabel = '"node' // trim(countStr) //'"'//' [label="Id_Cliente: ' // trim(idStr) // &
                        '\n '// trim(current%impresoraG_data%tipo_imagen) // '"]'
            
            write(unit, *) nodeLabel
            if (associated(current%next)) then 
                write(unit, "('node',i0,' -> node',i0)") count, count+1
            end if
            current => current%next
            count = count + 1
        end do
        write(unit, *) '}'
        close(unit)

        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')

      end subroutine grafica_impresionG

    !Gradica Lista clientes atendidos 
    subroutine grafica_lista_espera(this, filename)
        type(lista_lista_cliente_espera), intent(inout) :: this
        type(nodo_cliente_e), pointer :: currentNode
        type(sub_nodo), pointer :: currentSubNode
        
        integer :: unit
        integer :: nodeCount, subNodeCount
        
        character(len=*), intent(in) :: filename
        character(len=200) :: edgeLabel

        character(len=200) :: nodeLabel, idClienteStr
        character(len=200) :: imgG_Str, imgP_Str, countStr

        character(len=200) :: subNodeLabel, idClienteA_Str, tipoImgAStr
        character(len=200) :: nodeCountStr, subNodeCountStr


        nodeCount = 0
        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph ListaEspera {'
        write(unit, *) 'rankdir=LR;'
        write(unit, *) 'ranksep=1.0;'  ! Ajusta este valor según sea necesario

        write(unit, *) 'node [shape=box, style=filled, color=black, fillcolor=palegreen]'

        currentNode => this%head
        do while(associated(currentNode))
            write(countStr, '(i0)') nodeCount
            write(idClienteStr, '(i0)') currentNode%cliente_data_e%id_cliente_e
            write(imgG_Str, '(i0)') currentNode%cliente_data_e%img_g_e
            write(imgP_Str, '(i0)') currentNode%cliente_data_e%img_p_e
        
            nodeLabel = '"node' // trim(countStr) // '"' //'[label=" Id Cliente: ' // trim(idClienteStr) //  &
            '\n Nombre: ' // trim(currentNode%cliente_data_e%nombre_cliente) // '\n Img G: ' // &
            trim(imgG_Str) // '\n Img P: ' // trim(imgP_Str) // '"]'
            write(unit, *) trim(nodeLabel)

            !Graficar subnodos
            currentSubNode => currentNode%list_img_a
            subNodeCount = 0
            do while(associated(currentSubNode))
                write(nodeCountStr, '(i0)') nodeCount
                write(subNodeCountStr, '(i0)') subNodeCount
                write(idClienteA_Str, '(i0)') currentSubNode%img_data_a%id_cliente_a

                subNodeLabel = '"subNode' // trim(nodeCountStr) // '_' // trim(subNodeCountStr) // &
                '" [label="Id Cliente : ' // trim(idClienteA_Str) // '\n Tipo Img : ' // &
                trim(currentSubNode%img_data_a%tipo_img_a) // '"]'

                write(unit, *) trim(subNodeLabel)

                ! Conectar el primer subnodo con el nodo cliente respectivo
                if (subNodeCount == 0) then
                    write(unit, "('node',i0,':n -> subNode',i0,'_',i0,' [style=solid]') ") &
                    nodeCount, nodeCount, subNodeCount
                end if
                

                if (associated(currentSubNode%next)) then
                    write(unit, "('subNode',i0,'_',i0,' -> subNode',i0,'_',i0) ") &
                    nodeCount, subNodeCount, nodeCount, subNodeCount+1
                end if

                currentSubNode => currentSubNode%next
                subNodeCount = subNodeCount + 1
            end do

            ! Conectar con el siguiente nodo principal si existe
            if ((associated(currentNode%next)) .and. (.not. associated(currentNode%next, this%head))) then
                write(unit, "('node',i0,' -> node',i0) ") nodeCount, nodeCount+1
                write(unit, "('node',i0,' -> node',i0,' [constraint=false, style=dotted]') ") &
                nodeCount+1, nodeCount  ! Para enlace prev
            end if

            nodeCount = nodeCount + 1
            currentNode => currentNode%next
            if(associated(currentNode, this%head)) exit
            end do
            
            if (nodeCount > 0) then
                write(edgeLabel, "('node',i0,':n -> node0:n [constraint=false, style=dotted]')") nodeCount - 1
                write(unit, *) trim(edgeLabel)
                write(edgeLabel, "('node0:s -> node',i0,':s [constraint=false, style=dotted]')") nodeCount - 1
                write(unit, *) trim(edgeLabel)
            endif
    
        write(unit, *) '}'
        close(unit)
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
    end subroutine grafica_lista_espera

    subroutine grafica_clientes_atendidos(this, filename)
        type(list_finalizados), intent(inout) :: this
        type(Nodo_finalizado), pointer :: current
        character (len = *) :: filename
        integer :: unit, count
        character (len = 200) :: idCliente_Str, idVentanilla_Str, totalPasos_Str, count_Str 
        character (len = 200) :: imgG_recividas_Str, imgP_recividas_Str, nodeLabel

        open(newunit = unit, file = filename, status='replace')
        write(unit, *) 'digraph lista_clientes_atendidos {'
        write(unit, *) 'rankdir=LR;'
        write(unit, *) 'node [shape=record, style=filled, fillcolor=lightyellow]'

        current => this%head
        do while(associated(current))
            write(idCliente_Str, '(i0)') current%data_finalizado%id_cliente
            write(idVentanilla_Str, '(i0)') current%data_finalizado%id_ventanilla
            write(totalPasos_Str, '(i0)') current%data_finalizado%total_pasos
            write(imgG_recividas_Str, '(i0)') current%data_finalizado%img_g_recividas
            write(imgP_recividas_Str, '(i0)') current%data_finalizado%img_p_recividas
            write(count_Str, '(i0)') count
            nodeLabel = '"node' // trim(count_Str)//'"'//'[label="Id_cliente: '// &
            trim(idCliente_Str)// '\n Nombre: '//trim(current%data_finalizado%nombre_cliente) // &
            '\n id_ventanilla: '// trim(idVentanilla_Str) //'\n Img_g: '//trim(imgG_recividas_Str)// &
            '\n Img_p: '//trim(imgP_recividas_Str) // '\n totalPasos: '//trim(totalPasos_Str)//'"]'

            write(unit, *) nodeLabel
            if(associated(current%next))then
                write(unit, "('node',i0,' -> node', i0)") count, count+1
            end if
            current => current%next
            count = count + 1
        end do
        write(unit, *) '}'
        close(unit)

        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
    end subroutine grafica_clientes_atendidos


      ! Desencolamos uma imagen de nuestra cola de impresion P
      subroutine desencolar_imp_p(this, data_impresora_p)
          type(Cola_Impresion_P), intent(inout) :: this
          type(ImpresoraP), intent(out) :: data_impresora_p
          type(NodoImpresionP), pointer :: nodo_desencoladoP
  
          !Verificamos si nuestra cola Impresion P  esta vacia
          if(.not. associated(this%head)) then
              data_impresora_p%id_cliente = -1
              data_impresora_p%tipo_imagen = " "
              return
          end if
          
          nodo_desencoladoP => this%head
          data_impresora_p%id_cliente = nodo_desencoladoP%impresoraP_data%id_cliente
          data_impresora_p%tipo_imagen = nodo_desencoladoP%impresoraP_data%tipo_imagen
  
          this%head => this%head%next
          if (.not. associated(this%head)) then
              this%tail => null()
          end if
  
          !liberar el nodo desencolado
          deallocate(nodo_desencoladoP)
      end subroutine desencolar_imp_p
  
      ! Desencolamos una imagen de nuesta cola de impresion G
      subroutine desencolar_imp_g(this, data_impresora_g)
          type(Cola_Impresion_G), intent(inout) :: this
          type(ImpresoraG), intent(out) :: data_impresora_g
          type(NodoImpresionG), pointer :: nodo_desencoladoG
  
          !Verficamos si nuestra cola Impresion G esta vacia
          if (.not. associated(this%head))then
              data_impresora_g%id_cliente = -1
              data_impresora_g%tipo_imagen = " "
              return
          end if 
          nodo_desencoladoG => this%head
          data_impresora_g%id_cliente = nodo_desencoladoG%impresoraG_data%id_cliente
          data_impresora_g%tipo_imagen = nodo_desencoladoG%impresoraG_data%tipo_imagen
  
          this%head => this%head%next
          if(.not. associated(this%head)) then
              this%tail => null()
          end if
          !Liberar el nodo desencolado
          deallocate(nodo_desencoladoG)
      end subroutine desencolar_imp_g
  
      !Asignar imagen a cliente espera
      subroutine asignar_imagen_cliente(this, id_cliente_asociado, tipo_imagen)
          class(lista_lista_cliente_espera), intent(inout) :: this
          type(nodo_cliente_e), pointer :: aux ! Primer Nodo
          type(sub_nodo), pointer :: nuevo ! Segundo Nodo
          type(sub_nodo), pointer :: last ! buscar el ultimo nodo para insertar la nueva imagen
  
          integer, intent(in) :: id_cliente_asociado
          character (len = 1) , intent(in):: tipo_imagen
  
          aux => this%head
          do while(associated(aux))
              ! si Lista esperta id_cliente == id_cliente asociado => encontramos el cliente
              if(aux%cliente_data_e%id_cliente_e == id_cliente_asociado)then
                  
                  ! ahora asignemos memoria
                  allocate(nuevo)
                  nuevo%img_data_a%id_cliente_a = id_cliente_asociado
                  nuevo%img_data_a%tipo_img_a = tipo_imagen
  
                  nuevo%next => null() !este nodo es, por ahora, el último nodo de la lista
  
                  if(.not. associated(aux%list_img_a)) then
                      ! si eres el primero es decir no hay imagenes anteriormente incertadas a mi sub nodo
                      aux%list_img_a => nuevo
                  else
                      ! si ya hay imagenes incertadas a mi sub nodo
                      last => aux%list_img_a
                      do while(associated(last%next))
                          last => last%next
                      end do
                      last%next => nuevo
                  end if
                  ! Aqui al momento de que me asignes una imagen 
                  return ! sale del bucle
              end if
              aux => aux%next
          end do
      end subroutine asignar_imagen_cliente
  
      !Añadir imagen asociada cliente espera (desencolar - asingar imagen a cliente espera)
      subroutine add_img_a_cliente_espera(this, print_tail_p, print_tail_g)
          class(lista_lista_cliente_espera), intent(inout) :: this
           
          class(Cola_Impresion_P), intent(inout) :: print_tail_p
          type(ImpresoraP) :: dataP
  
          class(Cola_Impresion_G), intent(inout) :: print_tail_g
          type(ImpresoraG) :: dataG
  
          type(nodo_cliente_e), pointer :: temp, aux
          
          character(len=100) :: mensaje1
          character(len=100) :: mensaje2

          temp => this%head
          contador_ejecutar = contador_ejecutar + 1
          
          ! Desencolar imagen de impresora pequeña
          call desencolar_imp_p(print_tail_p, dataP)
        if(dataP%id_cliente /= -1)then    
            call asignar_imagen_cliente(this, dataP%id_cliente, 'P')
            !MENSAJE   Se completa la impresión de una imagen p -- se le entrega al cliente 1
            write(mensaje1, '(">> SE COMPLETA LA IMPRESION DE UNA IMAGEN ""P"" SE LE ENTREGA AL CLIENTE ", I0)') &
            dataP%id_cliente

            print *, mensaje1
        end if

        ! Incrementar en  img_p_copy + 1 si ingresamos una imagen
        if (dataP%tipo_imagen == 'P')then
            do while(associated(temp))
                if(temp%cliente_data_e%id_cliente_e == dataP%id_cliente) then
                    temp%cliente_data_e%img_p_copy = temp%cliente_data_e%img_p_copy + 1
                    exit 
                end if
                temp => temp%next
            end do
        end if

        ! Desencolar imagen de impresora grande
        if (contador_ejecutar >= 2)then
            call desencolar_imp_g(print_tail_g, dataG)
            if(dataG%id_cliente /= -1)then
                call asignar_imagen_cliente(this, dataG%id_cliente, 'G')
                !MENSAJE PARA ENTREGA DE IMAGENES GRANDES
                write(mensaje2, '(">> SE COMPLETA LA IMPRESION DE UNA IMAGEN ""G"" SE LE ENTREGA AL CLIENTE ", I0)') &
                dataG%id_cliente
                print *, mensaje2

                contador_ejecutar = 0

                ! Incrementar en  img_g_copy + 1 si ingresamos una imagen
                do while(associated(temp))
                    if(temp%cliente_data_e%id_cliente_e == dataG%id_cliente)then
                        temp%cliente_data_e%img_g_copy = temp%cliente_data_e%img_g_copy + 1
                        exit
                    end if
                    temp => temp%next
                end do

            end if  
        end if

     
        
        aux => this%head
        if (.not. associated(aux)) then
            print *, "La lista está vacía."
            return
        end if

        do
            ! Aquí incrementamos el paso para el cliente actual
            aux%cliente_data_e%paso = aux%cliente_data_e%paso + 1
            ! Verificar si hemos vuelto al inicio para evitar bucle infinito
            if (associated(aux%next, this%head)) exit
            aux => aux%next
        end do
        

      end subroutine add_img_a_cliente_espera

      !Añadir Clientes finalizados
      subroutine add_list_f(this, cliente_atendido)
        class(list_finalizados), intent(inout) :: this
        type(cliente_finalizado), intent(in) :: cliente_atendido  
        type(Nodo_finalizado), pointer :: tempNodo
        allocate(tempNodo)
        !print *, "Estas entrando aquiiii add list atendidos"
        tempNodo%data_finalizado%id_cliente =  cliente_atendido%id_cliente
        tempNodo%data_finalizado%id_ventanilla = cliente_atendido%id_ventanilla
        tempNodo%data_finalizado%nombre_cliente = cliente_atendido%nombre_cliente
        tempNodo%data_finalizado%img_p_recividas = cliente_atendido%img_p_recividas
        tempNodo%data_finalizado%img_g_recividas = cliente_atendido%img_g_recividas
        tempNodo%data_finalizado%total_pasos = cliente_atendido%total_pasos
        tempNodo%next => this%head
        this%head => tempNodo        
      end subroutine add_list_f

      !Eliminar Cliente en espera 
      subroutine delete_cliente_en_espera(this, id_cliente, data_cliente)
        class(lista_lista_cliente_espera), intent(inout) :: this
        integer, intent(in) :: id_cliente
        type(nodo_cliente_e), pointer :: currentNode , auxNode
        type(sub_nodo), pointer :: currentSubNodo , auxSubNode
        type(cliente_espera), intent(out) :: data_cliente ! copia que vamos a realizar antes de eliminar 

        currentNode => this%head
        do while(associated(currentNode))
            
            if(currentNode%cliente_data_e%id_cliente_e == id_cliente) then
                !hemos encontrado el cliente a eliminar
                !procedemos a realizar la copia
                data_cliente%id_cliente_e = currentNode%cliente_data_e%id_cliente_e
                data_cliente%id_ventanilla = currentNode%cliente_data_e%id_ventanilla
                data_cliente%nombre_cliente = currentNode%cliente_data_e%nombre_cliente
                data_cliente%img_g_e = currentNode%cliente_data_e%img_g_e
                data_cliente%img_p_e = currentNode%cliente_data_e%img_p_e
                data_cliente%paso = currentNode%cliente_data_e%paso
                
                ! procedemos a eliminar la lista de imagenes asociadas
                currentSubNodo => currentNode%list_img_a
                do while(associated(currentSubNodo))
                    !print *, "estras aqui a eliminar"
                    auxSubNode => currentSubNodo
                    currentSubNodo => currentSubNodo%next
                    deallocate(auxSubNode)        
                end do

                ! Ajustar punteros para eliminar el Nodo cliente espera 
                if(associated(currentNode%prev)) then
                    currentNode%prev%next => currentNode%next ! Si queremos eliminar el de enmedio
                else
                    this%head => currentNode%next
                    if(associated(this%head)) then
                        this%head%prev => this%tail
                    end if
                end if

                if(associated(currentNode%next)) then
                    currentNode%next%prev => currentNode%prev 
                else
                    this%tail => currentNode%prev
                    if(associated(this%tail)) then
                        this%tail%next => this%head
                    end if
                end if
                
                if(associated(this%head, this%tail))then
                    print *, "entras aqui"
                    this%head => null()
                    this%tail => null()
                else if(associated(currentNode, this%head))then
                    this%head => currentNode%next
                else if (associated(currentNode, this%tail)) then
                    this%tail => currentNode%prev
                end if

                deallocate(currentNode)
                exit 
            end if
            currentNode => currentNode%next
            if(associated(currentNode, this%head)) exit
        end do
      end subroutine delete_cliente_en_espera

        subroutine evaluar_cliente_espera(this, list_atendidos)
            type(lista_lista_cliente_espera), intent(inout) :: this
            type(list_finalizados), intent(inout) :: list_atendidos

            type(nodo_cliente_e), pointer :: currentCliente
            type(nodo_cliente_e), pointer :: nextCliente
            type(cliente_espera) :: data_cliente
            type(cliente_finalizado) :: data_atendido

            character (len = 100) :: mensaje1,  mensaje2
            character (len = 200) :: mensajeCompleto

            currentCliente => this%head

            if(.not. associated(currentCliente)) then
                return
            end if
            
            do while(associated(currentCliente))
                nextCliente => currentCliente%next
                if((currentCliente%cliente_data_e%img_g_e == currentCliente%cliente_data_e%img_g_copy) &
                .and. (currentCliente%cliente_data_e%img_p_e == currentCliente%cliente_data_e%img_p_copy))then
                    
                    call delete_cliente_en_espera(this, currentCliente%cliente_data_e%id_cliente_e, data_cliente)
                    ! rellenar los datos para la lista de clientes atendidos
                    data_atendido%id_cliente = data_cliente%id_cliente_e
                    data_atendido%id_ventanilla = data_cliente%id_ventanilla
                    data_atendido%nombre_cliente = data_cliente%nombre_cliente
                    data_atendido%img_g_recividas = data_cliente%img_g_e
                    data_atendido%img_p_recividas = data_cliente%img_p_e
                    data_atendido%total_pasos = data_cliente%paso
                    
                    call add_list_f(list_atendidos, data_atendido)
                    ! MENSAJE
                    ! El cliente 1 ya posee todas sus imágenes impresas y sale de la empresa registrando
                    !el tiempo total dentro de ella cantidad de pasos
                    write(mensaje1, '(">> EL CLIENTE ", I0 " OBTIENE TODAS SUS IMAGENES IMPRESAS")') &
                    data_atendido%id_cliente

                    write(mensaje2, '("INGRESA A LA LISTA DE CLIENTES ATENDIDOS REALIZO: ", "(", I0,")", " PASOS")') &
                    data_atendido%total_pasos
                    mensajeCompleto = trim(mensaje1) // " ---- "// trim(mensaje2)
                    print*, mensajeCompleto


                end if
            
                if(.not. associated(this%head)) then
                    exit ! Salir del ciclo si nextCliente no está asociado, evitando el acceso a memoria inválida
                endif
                
                if(associated(nextCliente, this%head))exit 
                currentCliente => nextCliente

            end do
        end subroutine evaluar_cliente_espera

        !show clientes atendidos
        subroutine show_clientes_atendidos(this)
            class(list_finalizados), intent(inout) :: this
            type(Nodo_finalizado), pointer :: tempCliente

            tempCliente => this%head
            do while(associated(tempCliente))
                print*, "----------- Cliente Antendido -----------"
                print *,"Id cliente: ", tempCliente%data_finalizado%id_cliente
                print *,"id ventanilla: ", tempCliente%data_finalizado%id_ventanilla
                print *,"nombre     ", tempCliente%data_finalizado%nombre_cliente
                print *,"img_p  ", tempCliente%data_finalizado%img_p_recividas
                print *, "img_g ",tempCliente%data_finalizado%img_g_recividas
                print *,"pasos  ", tempCliente%data_finalizado%total_pasos
                tempCliente => tempCliente%next
            end do

            if (.not. associated(this%head))then
                print *, "No hay clientes atendidos"
            end if

        end subroutine

      ! Ejecutar aplicacion
      subroutine ejecutar(this, colaClientes, listaClientesEspera, colaImpresionP, colaImpresionG, listaAtendidos)
          class(VentanillaList), intent(inout) :: this
          type(Tail), intent(inout) :: colaClientes
          type(NodoVentanilla), pointer :: tempVentanilla
          type(Client), pointer ::  cliente_to_ventanilla
          type(Client) :: nuevo_cliente
    
          !lista de clientes en espera
          type(lista_lista_cliente_espera), intent(inout) :: listaClientesEspera
  
          !cola de impresion imagenes grandes
          type(Cola_Impresion_P), intent(inout) :: colaImpresionP
  
          !cola de impresion imagenes grandes
          type(Cola_Impresion_G), intent(inout) :: colaImpresionG
  
          ! lisa de finalizados
          type(list_finalizados), intent(inout) :: listaAtendidos

          !Mensaje 
          character (len = 100) :: mensaje
 

          !Variables para generas imagenes y clientes aleatorios
          integer :: num_clientes,num_imgG, num_imgP, index_cliente, id_new, i
          real :: Rnum_cliente, Rnum_imgG, Rindex_nombreArray
          character(len= 50), dimension(5) :: nombre_aleatorio
          character(len = 50) :: name_aleatorio
          nombre_aleatorio = ["Robert ", "Alice  ", "Jose   ", "Luis   ", "Pedro  "]
          nombre_aleatorio(1) = "Robert"
          nombre_aleatorio(2) = "Alice"
          nombre_aleatorio(3) = "Jose"
          nombre_aleatorio(4) = "Luis"
          nombre_aleatorio(5) = "Pedro"

          call random_seed() ! Incializacion de número aleatorios
          ! numero aleatorio cliente
          call random_number(Rnum_cliente)
          num_clientes = int(Rnum_cliente * 4.0) ! 0 -3
          
          
          if (associated(colaClientes%tail)) then
            id_new = colaClientes%tail%data%id + 1
          else 
            id_new = 1 ! si la cola esta vacia
          end if
        
          
        
        if(num_clientes >= 1) then
            do i = 1 , num_clientes
                ! Imagenes grandes
                call random_number(Rnum_imgG)
                num_imgG = int(Rnum_imgG * 4.0) + 1 ! 1 - 4
                
                ! Imagenes pequenas   
                num_imgP = 4 - num_imgG

                ! indexe para obtener un nombre aleatorio de nuestro arreglo nombre_aleatorio
                call random_number(Rindex_nombreArray) 
                index_cliente = int(Rindex_nombreArray * 5.0) + 1 
                name_aleatorio = nombre_aleatorio(index_cliente)

                !pasarle data a mi cliente
                nuevo_cliente%id = id_new
                nuevo_cliente%name = trim(name_aleatorio)
                nuevo_cliente%img_p = num_imgP
                nuevo_cliente%img_g = num_imgG
                nuevo_cliente%copy_img_p = num_imgP
                nuevo_cliente%copy_img_g = num_imgG

                id_new = id_new + 1
                call encolar(colaClientes, nuevo_cliente)
            end do
        end if

        !MENSAJE
        contador_global = contador_global + 1
        write(mensaje, '("---------------------------- Paso ", I0, " ----------------------------")') contador_global
        print *, mensaje
        print *, "-----------------------------------------------------------------"

          ! evaluar si llego un cliente de espera a su cantidad de imagenes requeridas
          call evaluar_cliente_espera(listaClientesEspera, listaAtendidos)

          ! Agregar imagenes asociadas a clientes en espera
          if(associated(colaImpresionP%head) .or. associated(colaImpresionG%head))then
              call add_img_a_cliente_espera(listaClientesEspera, colaImpresionP, colaImpresionG)  
          else 
              !No hay nodos en colas de impresion pequeña
              !print *, "colas de impresion estan vacias."
          end if
  
          ! Añadir Cliente de esperta y imagenes a las colas y limpiar ventanilla
          call analizar_ventanilla(this, listaClientesEspera, colaImpresionP, colaImpresionG)
          
          !Agregar imagenes a la pila de cada ventanilla
          tempVentanilla => this%head
          do while(associated(tempVentanilla))
              if (associated(tempVentanilla%dataV%assing_client)) then  ! Si esta asociado un cliente
                  cliente_to_ventanilla => tempVentanilla%dataV%assing_client
                  call asignar_imagen(this, cliente_to_ventanilla)
              end if
              tempVentanilla => tempVentanilla%next
          end do
          
          !Asingar clientes en ventanillas Disponibles
          call asingar_cliente(this, colaClientes)
  
      end subroutine ejecutar
      

      !Montrar Ventanilla 
      subroutine show_ventanilla(this)
          class(VentanillaList), intent(in) :: this
          type(NodoVentanilla), pointer :: tempVent
          
          tempVent => this%head
          
          do while(associated(tempVent))
              print *, ""
              print *, "----------------------------------"
              print *, "Ventanilla :", tempVent%dataV%id
              print *, "----------------------------------"
              if (associated(tempVent%dataV%assing_client)) then
                  print *, "Id_cliente", tempVent%dataV%assing_client%id
                  print *, "Nombre", "    ", tempVent%dataV%assing_client%name
                  print *, "Img-p", tempVent%dataV%assing_client%img_p
                  print *, "Img-g", tempVent%dataV%assing_client%img_g
                  print *, "copia imagen P", tempVent%dataV%assing_client%copy_img_p
                  print *, "copia imagen G", tempVent%dataV%assing_client%copy_img_g
                  print *, "PASO ", tempVent%dataV%paso
  
                  call show_img(tempVent%dataV%pila_imgs)
  
              else
                  print *, "No esta asociado a una ventanilla"
              end if
              
  
              tempVent => tempVent%next
          end do
      end subroutine show_ventanilla
  
      subroutine show_img(this)
          class(PilaImagenes), intent(in) :: this
          type(NodoImagen), pointer :: currentNode
      
          currentNode => this%top  ! Comienza en el nodo superior
          print *, "Mostrando pila de imagenes:"
      
          ! Recorre la pila hasta que el puntero al siguiente nodo sea null
          do while(associated(currentNode))
              ! Imprime los detalles de la imagen en el nodo actual
              print *, "ID Imagen: ", currentNode%data_img%id_img
              print *, "Tipo Imagen: ", currentNode%data_img%type_img
              print *, "ID Cliente: ", currentNode%data_img%id_client
              print *, "---------------------------------"
              ! Mueve al siguiente nodo
              currentNode => currentNode%next
          end do
      
          if (.not. associated(this%top)) then
              print *, "La pila esta vacia."
          end if
      end subroutine show_img
      
  
    ! Reporte ordenar de mayor a menor imagenes grandes
    subroutine order_img_G(this, filename)
        class(list_finalizados), intent(inout) :: this
        type(Nodo_finalizado), pointer :: current, nextNode
        type(cliente_finalizado) :: temp_data
        integer :: count ,top_n, unit
        character (len = *), intent(in) :: filename
        character (len = 200) :: nodolabel, idStr, count_Str, imgG_recividas

        if (.not. associated(this%head)) then
            print *, "La lista esa vacia"
        end if

        top_n = 5
        count = 0

        current => this%head
        !Ordenamiento de mayor a menor
        do while(associated(current))
            nextNode => current
            do while(associated(nextNode))
                if (current%data_finalizado%img_g_recividas < nextNode%data_finalizado%img_g_recividas) then
                    temp_data = current%data_finalizado
                    current%data_finalizado = nextNode%data_finalizado
                    nextNode%data_finalizado = temp_data
                end if
                nextNode => nextNode%next
            end do
            current => current%next
        end do

        open(newunit=unit, file=filename, status='replace')
        write(unit, *) 'digraph Top5ImgG {'
        write(unit, *) 'rankdir=LR;' 
        write(unit, *) 'node [shape=box, style=filled, color=black, fillcolor=lightblue]'

        current => this%head
        count = 0
        do while(associated(current) .and. count < 5)
            write(idStr, '(i0)') current%data_finalizado%id_cliente
            write(imgG_recividas, '(i0)') current%data_finalizado%img_g_recividas
            write(count_Str, '(i0)') count + 1

            nodolabel = '"node' // trim(count_Str) // '" [label="Id Cliente: ' // trim(idStr) // &
                    '\n Nombre: '//trim(current%data_finalizado%nombre_cliente) // &
                    '\n img_g_recividas: ' // trim(imgG_recividas) // '"];'
            write(unit, *) trim(nodolabel)

            ! Si hay un siguiente pero no es el ultimo 
            if (associated(current%next) .and. count < 4) then
                write(unit, "('  node',i0,' -> node',i0)") count + 1, count + 2
            end if
            current => current%next
            count = count + 1
        end do
        write(unit, *) "}"
        close(unit)

        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
        print *, 'Graphviz file top5_imgG: ', trim(adjustl(filename)) // '.png'
    end subroutine order_img_G


    ! Reportes ordenar de mayor a menor imagenes pequeñas
    subroutine order_img_P(this, filename)
        class(list_finalizados), intent(inout) :: this
        type(Nodo_finalizado), pointer :: current, nextNode
        type(cliente_finalizado) :: temp_data
        integer :: count ,top_n, unit
        character (len = *), intent(in) :: filename
        character (len = 200) :: nodolabel, idStr, count_Str, imgP_recividas

        if (.not. associated(this%head)) then
            print *, "La lista esa vacia"
        end if

        top_n = 5
        count = 0

        current => this%head
        
        do while(associated(current))
            nextNode => current
            do while(associated(nextNode))
                if (current%data_finalizado%img_p_recividas < nextNode%data_finalizado%img_p_recividas) then
                    temp_data = current%data_finalizado
                    current%data_finalizado = nextNode%data_finalizado
                    nextNode%data_finalizado = temp_data
                end if
                nextNode => nextNode%next
            end do
            current => current%next
        end do

        open(newunit=unit, file= filename, status='replace')
        write(unit, *) 'digraph Top5ImgG {'
        write(unit, *) 'rankdir=LR;' 
        write(unit, *) 'node [shape=box, style=filled, color=black, fillcolor=green]'

        current => this%head
        count = 0
        do while(associated(current) .and. count < 5)
            write(idStr, '(i0)') current%data_finalizado%id_cliente
            write(imgP_recividas, '(i0)') current%data_finalizado%img_p_recividas
            write(count_Str, '(i0)') count + 1

            nodolabel = '"node' // trim(count_Str) // '" [label="Id Cliente: ' // trim(idStr) // &
                    '\n Nombre: '//trim(current%data_finalizado%nombre_cliente)// &
                    '\n img_p_recividas: ' // trim(imgP_recividas) // '"];'
            write(unit, *) trim(nodolabel)

            ! Si hay un siguiente pero no es el ultimo 
            if (associated(current%next) .and. count < 4) then
                write(unit, "('  node',i0,' -> node',i0)") count + 1, count + 2
            end if
            current => current%next
            count = count + 1
        end do
        write(unit, *) "}"
        close(unit)
        
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
        print *, 'Graphviz file top5_imgP: ', trim(adjustl(filename)) // '.png'

    end subroutine order_img_P

    !Reporte cliente que mas pasos estuvo
    subroutine cliente_con_masPasos(this, filename)
        class(list_finalizados), intent(inout) :: this
        type(Nodo_finalizado), pointer :: current, cliente_mayorP
        integer :: unit
        character(len=*), intent(in) :: filename
        character (len = 200) :: idStr, totalP_Str, labelnode
        if (.not. associated(this%head)) then
            print *, "La lista está vacía."
            return
        end if
    
        current => this%head
        cliente_mayorP => null()
        do while(associated(current))
            if (.not. associated(cliente_mayorP)) then
                cliente_mayorP => current
            else if (current%data_finalizado%total_pasos > cliente_mayorP%data_finalizado%total_pasos) then
                cliente_mayorP => current
            end if
            current => current%next
        end do
    
        if (.not. associated(cliente_mayorP)) then
            print *, "No se encontró un cliente válido."
            return
        end if
    
        ! Abrir archivo para escribir
        open(newunit=unit, file=filename, status='replace')
        write(unit, *) 'digraph ClienteConMasPasos {'
        write(unit, *) 'rankdir=LR;'
        write(unit, *) 'node [shape=box, style=filled, color="black", fillcolor="lightblue"]'
    
        write(idStr, '(I0)') cliente_mayorP%data_finalizado%id_cliente
        write(totalP_Str, '(I0)') cliente_mayorP%data_finalizado%total_pasos
        ! Escribir nodo del cliente con más pasos
        labelnode = '"Cliente" [label="Id Cliente: '// trim(idStr) // &
                            '\n Nombre: '// trim(cliente_mayorP%data_finalizado%nombre_cliente) // &
                            '\n Total Pasos: '// trim(totalP_Str) // '"];'
        write(unit, *) trim(labelnode)
        write(unit, *) "}"
        close(unit)

        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
        print *, "Graphviz file: ", trim(filename)
    end subroutine cliente_con_masPasos
    
    !Reporte de Datps de un cliente
    subroutine buscarcliente(this, id, filename)
        type(list_finalizados), intent(inout) :: this
        integer, intent(in) :: id
        type(Nodo_finalizado), pointer :: current
        character (len=*) :: filename

        integer :: unit 
        character (len = 200) :: idStr, idVentStr, imgG_Str, imgP_Str, totalP_Str, labelNode
        
        current => this%head

        open(newunit = unit, file= filename, status='replace')
        write(unit, *) 'digraph BuscarCliente {'
        write(unit, *) 'node [shape=record, style=filled, fillcolor=lightblue]'

        do while(associated(current))
            if (current%data_finalizado%id_cliente == id)then
                write(idStr, '(I0)') current%data_finalizado%id_cliente
                write(idVentStr, '(I0)') current%data_finalizado%id_ventanilla
                write(imgG_Str, '(I0)') current%data_finalizado%img_g_recividas
                write(imgP_Str, '(I0)') current%data_finalizado%img_p_recividas
                write(totalP_Str, '(I0)') current%data_finalizado%total_pasos
                labelNode =  '"Cliente" [label="Id Cliente: '// trim(idStr) // &
                '\n Ventanilla: '// trim(idVentStr) // &
                '\n Nombre: ' // trim(current%data_finalizado%nombre_cliente) // &
                '\n Imagenes Grandes Recibidas: '// trim(imgG_Str) // &
                '\n Imagenes Pequeñas Recibidas: ' // trim(imgP_Str)//  &
                '\n Total de Pasos: '// trim(totalP_Str)//'"];'
                write(unit, *) trim(labelNode)
                exit
            end if
            current => current%next
        end do
        write(unit, *) "}"
        close(unit)

        if (.not. associated(current))then
            print *, "No se encontró un cliente con el id: ", id
        else 
            call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
            print *, 'Datos de un cliente encontrados exitosamente: ', trim(adjustl(filename)) // '.png'
        end if

    end subroutine buscarcliente

  end module Fase_1
  

program pixel_print_studio
    use json_module
    use Fase_1
    implicit none
    
    !Json
    type(json_file) :: json   ! Se declara una variable del tipo json_file
    type(json_value), pointer :: p_list, p_cliente, p_atributos  ! Se declaran punteros a variables del tipo json_value
    type(json_core) :: jsonc  ! Se declara una variable del tipo json_core para acceder a las funciones básicas de JSON
    
    !Cliente
    type(Tail) :: reception_tail
    type(Client) :: cliente
    
    !Ventanilla
    type(VentanillaList) :: listVentanillas
    type(PilaImagenes) :: stack_img
    integer :: opcion, numVentanillas, i

    !Asignar cliente espera
    type(lista_lista_cliente_espera) :: listaDeListas

    !Mostrar cola de impresiones 
    type(Cola_Impresion_P) :: colaImpresion_P
    type(Cola_Impresion_G) :: colaImpresion_G

    !Instancias de la lista de clientes atendidos
    type(list_finalizados) :: list_atendidos
    type(cliente_finalizado) :: clienteFinalizado
    
    character(:), allocatable :: nombre  ! Se declara una cadena de caracteres que se asignará dinámicamente
    integer :: id_cliente, img_g, img_p

    type(cliente_espera) :: dataCliente

    integer :: j, size, opcionR, foundClient        ! Se declaran variables enteras
    logical :: found


    opcion = 0
    do while (opcion /= 7)
      print *, "--------------"
      print *, "Menu principal"
      print *, "--------------"
  
      print *, "[1] Cargar Clientes"
      print *, "[2] Estabecer Ventanillas"
      print *, "[3] Ejecutar Paso"
      print *, "[4] Estado en memoria de las estructuras"
      print *, "[5] Reportes"  
      print *, "[6] Acerca de"
      print *, "[7] Salir"
      
      print *, "-- Ingrese una opcion --"
      read *, opcion !leer la opcion
  
      select case (opcion)
      case (1)
        call json%initialize()    ! Se inicializa el módulo JSON
        call json%load(filename='data.json')  ! Se carga el archivo JSON llamado 'data.json'
        
        ! Se obtiene el tamaño del arreglo [] json
        call json%info('',n_children=size)
    
        call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
        call json%get('', p_list, found)
    
        do j = 1, size                          ! Se inicia un bucle sobre el número de elementos en el JSON
            call jsonc%get_child(p_list, j, p_cliente, found = found)  ! Se obtiene el i-ésimo hijo de listPointer

            nombre = ''
            id_cliente = 0
            img_g = 0
            img_p = 0

            ! Nombre
            call jsonc%get_child(p_cliente, 'nombre', p_atributos, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
            if (found) then                      
                call jsonc%get(p_atributos, nombre)
            end if
    
            ! Para 'id'
            call jsonc%get_child(p_cliente, 'id', p_atributos, found = found)
            if (found) then
                call jsonc%get(p_atributos, id_cliente)
            end if
    
            ! Para 'img_g'
            call jsonc%get_child(p_cliente, 'img_g', p_atributos, found = found)
            if (found) then
                call jsonc%get(p_atributos, img_g)       
            end if    
    
            ! Para 'img_p'
            call jsonc%get_child(p_cliente, 'img_p', p_atributos, found = found)
            if (found) then
                call jsonc%get(p_atributos, img_p)
            end if    
    
            call encolar(reception_tail, Client(id_cliente, trim(nombre), img_p, img_g, img_p, img_g))
        end do
    
        call json%destroy()  ! Se finaliza el módulo JSON
        
        print *, "Clientes cargados exitosamente"
        call show(reception_tail)

      case (2)
        print *, "Ingrese la cantida de ventanillas: "
        read *, numVentanillas
        do i = 1 , numVentanillas
          call add_ventanilla(listVentanillas, i)
        end do
        print *, "Ventanillas establecidas"
      case (3)
        call ejecutar(listVentanillas, reception_tail, listaDeListas, colaImpresion_P, colaImpresion_G, list_atendidos)
      case (4)
        call grafica_cola_recepcion(reception_tail, "test1.dot")
        call graficar_ventanillas(listVentanillas, "test2.dot")
        call grafica_impresionP(colaImpresion_P, "test3.dot")
        call grafica_impresionG(colaImpresion_G, "test4.dot")
        call grafica_lista_espera(listaDeListas, "test5.dot")
        call grafica_clientes_atendidos(list_atendidos, "test6.dot")
        print *, "ESTADO DE MEMORIA ACTUAL ESTABLECIDO CORRECTAMENTE"
      case (5)
        opcionR = 0
        do while(opcionR /= 5)
        print *, "1 >> Top 5 Clientes con mayor cantidad de imagenes grandes"
        print *, "2 >> Top 5 Clientes con mayor cantidad de imagenes pequenas"
        print *, "3 >> Informacion del Cliente que mas pasos estuvo en el sistema"
        print *, "4 >> Datos de un cliente"
        print *, "5 >> Volver al menu"
        print *, ">>>>> Ingrese una opcion <<<<<"
        read *, opcionR
        select case(opcionR)
        case (1)
            call order_img_G(list_atendidos, "top5imgG.dot")
        case (2)
            call order_img_P(list_atendidos, "top5imgP.dot")
        case (3)
            call cliente_con_masPasos(list_atendidos, "clienteConMasPasos.dot")
        case (4)
            foundClient = 0
            print *, "Ingrese el Id del cliente a buscar"
            read *, foundClient
            call buscarcliente(list_atendidos, foundClient, "datosCliente.dot")
        end select
        end do
      case (6)
        print *, "--- DATOS DEL ESTUDIANTE ---"
        print *, "Sergio Joel Rodas Valdez"
        print *, "Carne: 202200271"
        print *, "Seccion C"
      end select
    end do  

end program pixel_print_studio