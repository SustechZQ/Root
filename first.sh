#! /bin/bash
# Last Change:  2020-06-16 22:54:36
# 首先执行这个shell脚本，执行方式有2种
# 方法1：给脚本增加可执行权限
#        chmod +x first.sh
#        然后执行   ./first.sh  命令
#        注意前面有点和斜杠
# 方法2：直接在终端执行
#        sh firsh.sh
#
# 获取文件名及文件行数
# 存储在datafilename文件中
f_name=info.txt
pathdir_1=data_in
pathdir_2=data_out
############################################
if [ ! -d "$pathdir_1" ]; then
    mkdir $pathdir_1
    echo "缺少输入数据"
    echo "请在$pathdir_1文件夹内存储数据"
    exit
fi
if [ ! -s "$pathdir_1" ]; then
    echo "$pathdir_1是空目录"
    echo "请在$pathdir_1文件夹内存储数据"
    exit
fi
############################################
if [ ! -d "$pathdir_2" ]; then
    mkdir $pathdir_2
fi
if [ -s $pathdir_2 ]; then
    #echo "清空$pathdir_2文件夹"
    rm -rf $pathdir_2/*
fi
############################################
if [ ! -d "$pathdir_2/out" ]; then
    #echo "创建$pathdir_2/out文件夹"
    mkdir $pathdir_2/out
fi
if [ -s $pathdir_2/out ]; then
    rm -rf $pathdir_2/out/*
fi
############################################
if [ ! -d "$pathdir_2/out2" ]; then
    mkdir $pathdir_2/out2
fi
if [ -s $pathdir_2/out2 ]; then
    rm -rf $pathdir_2/out2/*
fi
############################################
if [ ! -d "$pathdir_2/fit_line1" ]; then
    mkdir $pathdir_2/fit_line1
fi
if [ -s $pathdir_2/fit_line1 ]; then
    rm -rf $pathdir_2/fit_line1/*
fi
############################################
if [ ! -d "$pathdir_2/fit_line6" ]; then
    mkdir $pathdir_2/fit_line6
fi
if [ -s $pathdir_2/fit_line6 ]; then
    rm -rf $pathdir_2/fit_line6/*
fi
############################################
ls ./$pathdir_1 | wc -w >> $pathdir_2/$f_name
files=$(ls $pathdir_1)
############################################
totalfile=$(ls ./$pathdir_1 | wc -w)
j=0
############################################
for filename in $files
do
    j=$(($j+1))
    outname=$pathdir_2/out/$filename
    echo $filename >> $pathdir_2/$f_name
    #wc -l $pathdir_1/$filename >> $pathdir_2/$f_name
    # 处理原始数据
    sed 's/\t\t/\t-100\t/g' $pathdir_1/$filename > $outname
    s1=$(file $outname)
    s2="CRLF"
    result=$(echo $s1 | grep -i "$s2")
    if [ "$result" != "" ];then
        dos2unix $outname >> /dev/null 2>&1
    fi
    sed -i 's/\t\t/\t-100\t/g' $outname
    sed -i 's/\t\t/\t-100\t/g' $outname
    sed -i 's/\t\t/\t-100\t/g' $outname
    sed -i 's/ *//g' $outname
    filedataset=$(cat $outname | head -n 3 | tail -n 1)
    fileformat=""
    i=0
    for data in $filedataset
    do
        i=$(($i+1))
        if [ "$data" != " " ];then
            if [ "$data" != "" ];then
                if [ "$data" != "-100" ];then
                    if [ "$fileformat" == "" ];then
                        fileformat="\$$i"
                    else
                        fileformat=$fileformat,"\$$i"
                    fi
                fi
            fi
        fi
    done
    echo "[ $j / $totalfile ]  $filename "
    echo "..."
    echo "OK"
#    echo $filedataset
#    echo $fileformat
    cat $outname | awk "{print $fileformat}" >> $outname.bak
    rm -rf $outname
    cat $outname.bak | while read linetxt
    do
        linetxt2=$(echo $linetxt | sed 's/[ ]*$//g')
        echo $linetxt2 >> $outname
    done
    rm -rf $outname.bak
    outformatflag=0
    firstlineformat_1=$(cat $outname | head -n 1 | awk '{print $NF}')
    firstlineformat_2=$(cat $outname | head -n 1 | awk '{print $(NF-1)}')
    s3=stress
    s4=strain
    result1=$(echo $firstlineformat_1 | grep -i "$s3")
    result2=$(echo $firstlineformat_2 | grep -i "$s4")
    if [ "$result1" == "" ];then
        outformatflag=1
    fi
    if [ "$result1" == "" ];then
        outformatflag=1
    fi
    sed -i '1d' $outname
    if [ "$outformatflag" == "0" ];then
        awk '{print $(NF-1),"\t",$NF}' $outname > $outname.bak
    fi
    if [ "$outformatflag" == "1" ];then
        awk '{print $NF,"\t",$(NF-1)}' $outname > $outname.bak
    fi
    dataset=$(tail -n 1 $outname.bak)
    for datanum in $dataset
    do
        flag=0
        if [ "$datanum" == "-100" ];then
            flag=1
        fi
    done
    if [ "$flag" == "1" ];then
        sed -i '$d' $outname.bak
    fi
    mv $outname.bak $outname
    cat $outname | wc -l >> $pathdir_2/$f_name
    head -1 $outname | awk '{print NF}' >> $pathdir_2/$f_name
done
############################################

# 自动编译源代码

FOR_SRC=calc.f95
FOR_EXE=calc

if [ -f "$FOR_SRC" ]; then
    #echo "存在$FOR_SRC文件"
    if [ -f "$FOR_EXE" ]; then
        #echo "存在$FOR_EXE文件"
        which gfortran >> /dev/null
        if [ $? -eq 0 ];then
            #echo "重新编译可执行文件"
            gfortran $FOR_SRC -o $FOR_EXE
            if [ $? -eq 0 ];then
                #echo "如果可执行文件没有执行权限,增加权限后执行"
                if [ -x $FOR_EXE ];then
                    ./$FOR_EXE
                else
                    chmod +x $FOR_EXE
                    ./$FOR_EXE
                fi
            else
                echo "代码有错误，请手动编译调试"
                return
            fi
        else
            #echo "如果可执行文件没有执行权限,增加权限后执行"
            if [ -x $FOR_EXE ];then
                ./$FOR_EXE
            else
                chmod +x $FOR_EXE
                ./$FOR_EXE
            fi

        fi
    else
        #echo "用源代码$FOR_SRC编译可执行文件"
        which gfortran >> /dev/null
        if [ $? -eq 0 ];then
            #echo "存在gfortran编译器"
            gfortran $FOR_SRC -o $FOR_EXE
            if [ $? -eq 0 ];then
                #echo "如果可执行文件没有执行权限,增加权限后执行"
                if [ -x $FOR_EXE ];then
                    ./$FOR_EXE
                else
                    chmod +x $FOR_EXE
                    ./$FOR_EXE
                fi
            else
                echo "代码有错误，请手动编译调试"
                return
            fi
        else
            echo ""
            echo "不存在gfortran编译器"
            echo "安装gfortran编译器"
            echo "安装命令：sudo apt-get install gfortran"
            echo "安装gfortran编译器后,再执行此脚本"
            echo ""
        fi
    fi
elif [ -f "$FOR_EXE" ]; then
    #echo "如果可执行文件没有执行权限,增加权限后执行"
    if [ -x $FOR_EXE ];then
        ./$FOR_EXE
    else
        chmod +x $FOR_EXE
        ./$FOR_EXE
    fi
else
    echo "不存在源代码$FOR_SRC文件"
    echo "不存在$FOR_EXE可执行文件"
    echo "请补充源代码$FOR_SRC文件"
    echo "或补充$FOR_EXE可执行文件"
    echo "补充相应文件后再执行此脚本"
    exit
fi

############################################
