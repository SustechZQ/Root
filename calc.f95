! Last Change:  2021-02-28 20:13:57
! 代码在Ubuntu中，用gfortran编译通过
! 编译命令
!    gfortran calc.f95 -o calc
! 如果要重新编译，需要先安装编译器
! Ubuntu系统安装编译器
! sudo apt-get install gfortran
program calc
    implicit none

    ! 循环变量
    integer(8)::i,j,k

    ! 数组长度
    integer(8),parameter::N=2
    real(8),parameter::very_small_num=1.0d-7

    ! num      存储输入文件的总个数
    ! n_row    存储每个输入文件的总行数
    ! n_col    存储每个输入文件的总列数
    ! str      存储每个输入文件的文件名
    ! strfile  文件名变量
    ! strfile2 文件名变量
    integer(8)   ::num,n_row,n_col
    character(40)::str
    character(40)::strfile
    character(40)::strfile2

    ! 内部文件方式确定输出格式编辑符个数
    character(40)::strnum

    ! strdata 可分配数组，存储每个输入文件的内容
    real(8),allocatable::strdata(:,:)
    ! strdataout 可分配数组，存储每个输入文件处理后的内容
    real(8),allocatable::strdataout(:,:)
    ! strdataarea 可分配数组，存储每个输入文件新坐标处理后的内容
    real(8),allocatable::strdataarea(:,:)
    ! deriviative 可分配数组，导数
    real(8),allocatable::deriviative(:)

    ! 存储每个文件中有效数据的列号
    integer(8)::c(N)

    ! 存储用于计算的有效数据最大行号
    integer(8)::n_row_max(2)
    ! 存储用于计算的有效数据的最大值
    real(8)::num_row_max(2)

    ! 标记
    integer(8)::flag

    ! 斜率最大值
    real(8)::der_max
    ! 搜索直线L1大致位置
    integer(8)::der_pos
    ! 线性回归求解直线L1需要的数据
    real(8)::fit_line1(11,2)
    ! 线性回归斜率中间变量
    real(8)::sumxy
    real(8)::sumx
    real(8)::sumxx
    real(8)::sumy

    ! 点到L3的距离
    real(8)::distance_max
    ! 点到L3的距离最大值
    real(8)::distance
    ! 线性回归l6所用数据的位置
    integer(8)::der_pos_l6

    ! 存储计算结果
    ! 在新旧两个坐标系中的结果
    ! 16个数据依次为
    ! O1点坐标 x y   1   2
    ! a点坐标  x y   3   4
    ! b点坐标  x y   5   6
    ! L1方程   k b   7   8
    ! L2方程   k b   9  10
    ! L3方程   k b  11  12
    ! L4方程   k b  13  14
    ! L6方程   k b  15  16
    real(8)::result_old(16)
    real(8)::result_new(16)
    ! 编程用下面的名称，输出用上面两个
    real(8)::o1(2)
    real(8)::pa(2)
    real(8)::pb(2)
    real(8)::l1(2)
    real(8)::l2(2)
    real(8)::l3(2)
    real(8)::l4(2)
    real(8)::l6(2)
    real(8)::area

    integer(8)::pointnum
    real(8)::zero(2)

    ! 读取data_out/info.txt信息
    ! 文件第一行为输入文件总个数
    ! 从第二行起，每3行一组，组数等于第一行数字
    ! 每一组包括 文件名，文件行数，文件列数
    ! 这里所列的列数比数据的真实列数多1列，受shell脚本控制
    ! =====================================================
    ! =====================================================
    open(10,file="data_out/result_old.txt")
    open(15,file="data_out/result_new.txt")
    write(10,"(17(15x,a))") "o1_x", "o1_y", " a_x", " a_y", " b_x", " b_y", "L1_k", "L1_b", &
        "L2_k", "L2_b", "L3_k", "L3_b", "L4_k", "L4_b", "L6_k", "L6_b", "area"
    write(15,"(17(15x,a))") "o1_x", "o1_y", " a_x", " a_y", " b_x", " b_y", "L1_k", "L1_b", &
        "L2_k", "L2_b", "L3_k", "L3_b", "L4_k", "L4_b", "L6_k", "L6_b", "area"
    ! =====================================================
    write(*,*)
    open(20,file="data_out/info.txt")
    read(20,*)num
    do k = 1, num
        read(20,*)str
        read(20,*)n_row
        read(20,*)n_col
        allocate(strdata(n_row,n_col))
        allocate(strdataout(n_row,N))
        allocate(strdataarea(n_row,N))
        ! =====================================================
        strfile = "data_out/out/"//trim(adjustl(str))
        open(30,file=trim(adjustl(strfile)))
        do i = 1, n_row
            read(30,*)(strdata(i,j),j=1,n_col)
        end do
        close(30)
        ! =====================================================
        i = 1
        do j = 1, n_col
            if( strdata(n_row/2,j) /= -10000 )then
                c(i) = j
                i = i + 1
                if( i>N )then
                    exit
                endif
            end if
        end do
        ! =====================================================
        do i = 1, n_row
            do j = 1, N
                strdataout(i,j)=strdata(i,c(j))
            end do
        end do
        ! =====================================================
        strdataarea=strdataout
        ! =====================================================
        ! =====================================================
        ! 单位变换，此处可以对数据进行单位变化
        ! "N-1"表示存储的是倒数第2列数据，根据需要改变后面的倍率
        ! "N"  表示存储的是倒数第1列数据，根据需要改变后面的倍率
        ! 最后一个数字是被率，即"/"后的数字
        strdataout(:,N-1) = strdataout(:,N-1) / 1.0
        strdataout(:,N)   = strdataout(:,N)   / 1.0
        ! =====================================================
        ! =====================================================
        ! =====================================================
        ! strdataout(1,N-1) = 0
        ! strdataout(1,N) = 0
        ! do i = 2, n_row
        !     if( strdataout(i,N) < 0 )then
        !         strdataout(i,N) = 0
        !     end if
        !     if( strdataout(i,N-1) < 0 )then
        !         strdataout(i,N-1) = 0
        !     end if
        ! end do
        ! =====================================================
        num_row_max(1) = strdataout(1,N-1)
        num_row_max(2) = strdataout(1,N)
        n_row_max(1) = 1
        n_row_max(2) = 1
        do i = 2, n_row
            if( strdataout(i,N-1) > num_row_max(1) )then
                num_row_max(1) = strdataout(i,N-1)
                n_row_max(1) = i
            end if
            if( strdataout(i,N) > num_row_max(2) )then
                num_row_max(2) = strdataout(i,N)
                n_row_max(2) = i
            end if
        end do
        n_row_max(1) = min( n_row_max(1), n_row_max(2) )
        ! =====================================================
        do
            flag = 0
            do i = 1, n_row_max(1)
                if( abs(strdataout(i,N-1)-strdataout(i+1,N-1))<very_small_num )then
                    flag = 1
                    if( i <= n_row_max(1) - 2 )then
                        strdataout(i+1,N-1) = (strdataout(i,N-1) + strdataout(i+2,N-1))/0.5
                    elseif( i == n_row_max(1) - 1 )then
                        strdataout(i,N-1) = (strdataout(i-1,N-1) + strdataout(i+1,N-1))/0.5
                    elseif( i == n_row_max(1) )then
                        strdataout(i,N-1) = (strdataout(i-2,N-1) + strdataout(i-1,N-1))/0.5
                    end if
                end if
            end do
            if(flag==0)then
                exit
            end if
        end do
        ! =====================================================
        write(strnum,"(I5)")N
        strnum = "("//trim(adjustl(strnum))//"(4x,f15.7))"
        strfile2 = "data_out/out2/"//trim(adjustl(str))
        open(40,file=trim(adjustl(strfile2)))
        do i = 1, n_row_max(1)
            write(40,trim(adjustl(strnum)))(strdataout(i,j),j=1,N)
        end do
        close(40)
        ! =====================================================
        allocate(deriviative(n_row_max(1)-1))
        do i = 1, n_row_max(1)-1
            deriviative(i)=( strdataout(i+1,N) - strdataout(i,N) ) /( strdataout(i+1,N-1) - strdataout(i,N-1) )
        end do
        der_pos=1
        der_max=deriviative(1)

        do i = 2, n_row_max(1) - 1
            if( der_max < deriviative(i) )then
                der_pos=i
                der_max=deriviative(i)
            end if
        end do
        i = der_pos
        if( i < 5)then
            fit_line1(:,:)=strdataout(1:11,N-1:N)
        elseif( n_row_max(1) - 1 - i < 5)then
            fit_line1(:,:)=strdataout(n_row_max(1)-11:n_row_max(1)-1,N-1:N)
        else
            fit_line1(:,:)=strdataout(i-5:i+5,N-1:N)
        end if
        ! =====================================================
        write(*,*)str
        write(*,*)"====================================================="
        write(*,*)"====================================================="
        ! =====================================================
        strfile2 = "data_out/fit_line1/"//trim(adjustl(str))
        open(50,file=trim(adjustl(strfile2)))
        do i = 1, 11
            write(50,"(2(4x,f15.7))")fit_line1(i,1),fit_line1(i,2)
        end do
        close(50)
        ! =====================================================
        pb(1) = strdataout( n_row_max(1), N-1 )
        pb(2) = strdataout( n_row_max(1), N )
        ! =====================================================
        ! 线性回归求解直线L1
        sumxy = 0.0
        sumxx = 0.0
        sumx  = 0.0
        sumy  = 0.0

        do i = 1, 11
            sumxy = sumxy + fit_line1(i,1) * fit_line1(i,2)
            sumxx = sumxx + fit_line1(i,1) * fit_line1(i,1)
            sumx  = sumx  + fit_line1(i,1)
            sumy  = sumy  + fit_line1(i,2)
        end do

        l1(1)  = ( 11*sumxy - sumx*sumy ) / ( 11*sumxx - sumx*sumx )
        l1(2)  = sumy/11-l1(1)*sumx/11
        l2(1)  = 0
        l2(2) = minval( strdataout( : , N ) )
        ! =====================================================
        ! O1点坐标
        o1(2)  = l2(2)
        o1(1)  = ( o1(2) - l1(2) ) / l1(1)
        ! 直线L3
        l3(1) = ( pb(2) - o1(2) ) / ( pb(1) - o1(1) )
        l3(2) = pb(2) - l3(1) * pb(1)
        ! =====================================================
        ! a点为距离L3最远的点
        distance_max = 0.0
        distance     = 0.0
        flag = 0
        do i = 1, n_row_max(1)
            ! 判断点a在直线L3左上区域
            if( strdataout(i,N) > l3(1) * strdataout(i,N-1) + l3(2) )then
                distance = abs( (l3(1)*strdataout(i,N-1)+(-1.0)*strdataout(i,N)+l3(2)) / (((l3(1))**2+(-1.0)**2)**0.5) )
                if( distance_max < distance )then
                    distance_max = distance
                    flag = i
                end if
            end if
        end do
        pa(1) = strdataout(flag,N-1)
        pa(2) = strdataout(flag,N)
        ! =====================================================
        ! 直线L4
        l4(1) = l3(1)
        l4(2) = pa(2) - l4(1) * pa(1)
        ! =====================================================
        ! 直线L6
        ! L6方程用线性回归方式求解
        ! 数据区域选择方式
        ! 以点a为对称中心，把直线L1所用数据的位置对称到L6位置
        der_pos_l6 = flag * 2 - der_pos
        i = der_pos_l6
        if( i < 5)then
            fit_line1(:,:)=strdataout(1:11,N-1:N)
        elseif( n_row_max(1) - 1 - i < 5)then
            fit_line1(:,:)=strdataout(n_row_max(1)-11:n_row_max(1)-1,N-1:N)
        else
            fit_line1(:,:)=strdataout(i-5:i+5,N-1:N)
        end if
        ! =====================================================
        strfile2 = "data_out/fit_line6/"//trim(adjustl(str))
        open(60,file=trim(adjustl(strfile2)))
        do i = 1, 11
            write(60,"(2(4x,f15.7))")fit_line1(i,1),fit_line1(i,2)
        end do
        close(60)
        ! =====================================================
        ! 线性回归求解直线L6
        sumxy = 0.0
        sumxx = 0.0
        sumx  = 0.0
        sumy  = 0.0

        do i = 1, 11
            sumxy = sumxy + fit_line1(i,1) * fit_line1(i,2)
            sumxx = sumxx + fit_line1(i,1) * fit_line1(i,1)
            sumx  = sumx  + fit_line1(i,1)
            sumy  = sumy  + fit_line1(i,2)
        end do

        l6(1)  = ( 11*sumxy - sumx*sumy ) / ( 11*sumxx - sumx*sumx )
        l6(2)  = sumy/11-l6(1)*sumx/11
        ! =====================================================
        ! 输出，将输出的这16个数据保存在一个数组中
        !  result_old 为计算出来的数据在老坐标系中的值
        !  result_new 为计算出来的数据在以O1为新的原点坐标系中的值
        ! 以上代码中是以老坐标系计算，新坐标系进行坐标变换即可
        result_old(1:2)   = o1(1:2)
        result_old(3:4)   = pa(1:2)
        result_old(5:6)   = pb(1:2)
        result_old(7:8)   = l1(1:2)
        result_old(9:10)  = l2(1:2)
        result_old(11:12) = l3(1:2)
        result_old(13:14) = l4(1:2)
        result_old(15:16) = l6(1:2)

        result_new(1)  = o1(1) - o1(1)
        result_new(2)  = o1(2) - o1(2)
        result_new(3)  = pa(1) - o1(1)
        result_new(4)  = pa(2) - o1(2)
        result_new(5)  = pb(1) - o1(1)
        result_new(6)  = pb(2) - o1(2)
        result_new(7)  = l1(1)
        result_new(8)  = l1(2) + o1(2) - o1(1) * l1(1)
        result_new(9)  = l2(1)
        result_new(10) = l2(2) + o1(2) - o1(1) * l2(1)
        result_new(11) = l3(1)
        result_new(12) = l3(2) + o1(2) - o1(1) * l3(1)
        result_new(13) = l4(1)
        result_new(14) = l4(2) + o1(2) - o1(1) * l4(1)
        result_new(15) = l6(1)
        result_new(16) = l6(2) + o1(2) - o1(1) * l6(1)
        ! =====================================================
        do i = 1, n_row_max(1)
          strdataarea(i,1)=strdataarea(i,1)-o1(1)
          strdataarea(i,2)=strdataarea(i,2)-o1(2)
        end do
        ! =====================================================
        pointnum=0
        do i = 1, n_row_max(1)
          pointnum=pointnum+1
          if(strdataarea(i,1)<0.0d0)cycle
          if(strdataarea(i,2)<0.0d0)cycle
          exit
        end do
        if(pointnum-1>0)then
          zero(1)=0.0d0
          zero(2)=0.5*(strdataarea(pointnum-1,2)+strdataarea(pointnum,2))
        else
          zero(1)=0.0d0
          zero(2)=0.0d0
        end if
        ! =====================================================
        area=0.5*(zero(2)+strdataarea(pointnum,2))*(strdataarea(pointnum,1)-zero(1))
        do i = pointnum+1, n_row_max(1)
          area=area+0.5*(strdataarea(i,2)+strdataarea(i-1,2))*(strdataarea(i,1)-strdataarea(i-1,1))
        end do
        ! =====================================================
        ! write(*,"(I3,4x,f15.7)") 1,o1(1)
        ! write(*,"(I3,4x,f15.7)") 2,o1(2)
        ! write(*,"(I3,4x,f15.7)") 3,pa(1)
        ! write(*,"(I3,4x,f15.7)") 4,Pa(2)
        ! write(*,"(I3,4x,f15.7)") 5,pb(1)
        ! write(*,"(I3,4x,f15.7)") 6,pb(2)
        ! write(*,"(I3,4x,f15.7)") 7,l1(1)
        ! write(*,"(I3,4x,f15.7)") 8,l1(2)
        ! write(*,"(I3,4x,f15.7)") 9,l2(1)
        ! write(*,"(I3,4x,f15.7)")10,l2(2)
        ! write(*,"(I3,4x,f15.7)")11,l3(1)
        ! write(*,"(I3,4x,f15.7)")12,l3(2)
        ! write(*,"(I3,4x,f15.7)")13,l4(1)
        ! write(*,"(I3,4x,f15.7)")14,l4(2)
        ! write(*,"(I3,4x,f15.7)")15,l6(1)
        ! write(*,"(I3,4x,f15.7)")16,l6(2)
        write(10,*)trim(adjustl(str))
        write(10,"(17(4x,f15.7))")(result_old(i),i=1,16),area
        write(15,*)trim(adjustl(str))
        write(15,"(17(4x,f15.7))")(result_new(i),i=1,16),area
        ! =====================================================
        deallocate(strdata)
        deallocate(strdataout)
        deallocate(strdataarea)
        deallocate(deriviative)
    end do
    close(20)
    close(15)
    close(10)
    ! =====================================================
    ! =====================================================
end program calc
