module reproduction
    implicit none
    private
    public :: repro

contains

    subroutine repro(nppa, height1, seed_mass, n_seed, seed_bank)
        use global_par

        ! Declaração das variáveis de entrada
        real(r_8), intent(in) :: height1
        real(r_4), intent(in) :: nppa
        real(r_8), intent(inout) :: seed_bank  ! Banco de sementes passado como inout

        ! Declaração das variáveis de saída
        real(r_8), intent(out) :: n_seed
        real(r_8), intent(out) :: seed_mass

        ! Variáveis internas
        real(r_8) :: height
        real(r_8) :: npp_rep
        real(r_8) :: new_seed_bank
        real(r_8) :: seed_mass_log

        ! Calculando a massa da semente
        height = height1  ! Altura da planta em metros
        npp_rep = nppa * 0.90  ! 4% do NPP disponível para reprodução
        print *, "valor de npp_rep:", npp_rep
        print *, "valor de nppa", nppa

        ! Nova fórmula para a massa da semente em miligramas (mg)
        seed_mass_log = 0.039 * height + 1.1951

        ! Converte a massa da semente de escala logarítmica para normal (linear)
        seed_mass = 10.0 ** seed_mass_log

        ! Convertendo a massa da semente para quilogramas (kg) - caso seja preciso
        !seed_mass = seed_mass / 1.0e6
        
        ! Imprime a massa da semente
        print *, "Altura da planta:", height, "m, Massa da semente:", seed_mass, "mg"

        ! Verificando se a massa da semente é menor que o limite de 8 mg
        if (seed_mass < 8.0) then
            seed_mass = 8.0
        endif

        ! Calculando o número de sementes
        n_seed = npp_rep / seed_mass
        n_seed = int(n_seed)  ! Garantindo que o número de sementes seja um valor inteiro

        ! Garantindo que o número de sementes seja um valor inteiro
        ! Garantir que n_seed seja pelo menos 1 se houver produção
        if (n_seed < 1 .and. npp_rep > 0) then
            n_seed = 1
        endif
        
        print*, "numero de sementes produzidas:", n_seed
        print *, "Tamanho do banco de sementes antes da produção:", seed_bank

        ! Atualizando o banco de sementes
        new_seed_bank = seed_bank + n_seed
        seed_bank = new_seed_bank

        ! Evitar que seed_bank assuma valores não realistas
        if (seed_bank < 0) then
            seed_bank = 0
        endif

        ! Imprime para depuração
        print *, "Tamanho do banco de sementes após a produção:", seed_bank

    end subroutine repro

end module reproduction
