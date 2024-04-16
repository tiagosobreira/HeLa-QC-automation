param ($instrument)


####################################################################################################################################
#                        How to run the script                                                                                     #
#                                                                                                                                  #
# powershell.exe -ExecutionPolicy Bypass .\monitor_HeLa_Folder_and_run_PD.ps1 -instrument Eclipse_1                                #
#                                                                                                                                  #
####################################################################################################################################


##
#Range of how old the raw file can be to be processed
$start_time=(Get-Date) - (new-timespan -day 20000)
$end_time= Get-Date

Add-Content -Path .\log_HeLa_QC.txt -Value "$end_time MS $instrument" -encoding UTF8

################################################################
#                                                              #
#                     HeLa QC                                  #
#
################################################################

######
###Test if PD is running
$raw_des_folder = "D:\Automate_QC\*.raw"
if (Test-Path $raw_des_folder) { 
   Write-Host "PD is running"
   exit
}

##
#folder with the HeLa ms files
$folder = "\\172.25.100.115\proteomics\Automation_scripts\Hela_"+$instrument+"\*.raw"


##
#Workstation working folder
$des_folder = "D:\Automate_QC\"

$files = Get-ChildItem $folder -Recurse

foreach ($f in $files){
      
    if($f.lastwritetime -ge $start_time -and $f.lastwritetime -le $end_time) { 

       $filename = (Get-Item $f).Basename
       $Stat_files = $des_folder+$filename+"_ResultStatistics.txt"
       $Stat_files1 = $des_folder+$instrument+"_ResultStatistics\"+$filename+"_ResultStatistics.txt"
       
       if (Test-Path $Stat_files1){
           ##
           #If file exists, data already analyzed!
           continue
       }
       
       ###
       #This is to avoid process the same file twice because of Windows task scheduler bug
       Add-Content -Path $Stat_files1 -Value "Lock"
       
       ###
       #Copying the raw to the local disk 
       copy-item  $f.fullname $des_folder
       Start-Sleep 30
       $full_filename = split-path $f -Leaf
       ###
       #Running PD
       & "C:\Program Files\Thermo\Proteome Discoverer Daemon 2.5\System\Release\DiscovererDaemon.exe" -c Rawfiles -a Rawfiles $des_folder$full_filename -e Rawfiles ANY '"D:\Automate_QC\config_files\PWF_Tribrid_SequestHT_MSAmanda_Percolator_HeLa.pdProcessingWF";"D:\Automate_QC\config_files\CWF_Comprehensive_Enhanced Annotation_HeLa.pdConsensusWF"'

       ###
       #Waiting PD output
       $PD_start_time = Get-Date
       
       while (!(Test-Path $Stat_files)) { 
            Start-Sleep 10

            #####
            #if PD crash this will end the script after 1 hour            
            $current_time = Get-Date
            $diff = New-TimeSpan -Start $PD_start_time -End $current_time
            if ($diff.Hours -gt 1 ){
                Remove-Item $f
                Remove-Item $des_folder"*$filename*"
                Remove-Item $Stat_files1
                $current_date = Get-Date
                Add-Content -Path .\PD_error.txt -Value "PD error $f  $current_date" -encoding UTF8
                exit
            }
       }
       Start-Sleep 30
       
       $protein = 0
       $peptides = 0
       $psms = 0
       $msms = 0

       ####
       ##Parser ResultStatistics file 
       Write-Host "Parser ResultStatistics file"
       $file_data = Get-Content $Stat_files | Where-Object {$_ -like "*Protein Groups - # Proteins*"}
       $protein = ($file_data[0] -split "`t")[-1]

       $file_data = Get-Content $Stat_files | Where-Object {$_ -like '*Peptide Groups - # Proteins*'}
       $peptides = ($file_data[0] -split "`t")[-1]
       
       $file_data = Get-Content $Stat_files | Where-Object {$_ -like '*PSMs - # Protein Groups*'}
       $psms = ($file_data[0] -split "`t")[-1]

       $file_data = Get-Content $Stat_files | Where-Object {$_ -like '*Spectrum Info - # PSMs*'}
       $msms = ($file_data -split "`t")[-1]


       if ($protein -notmatch '\d'){
           $protein = 0
       }

       if ($peptides -notmatch '\d'){
           $peptides = 0
       }

       if ($psms -notmatch '\d'){
           $psms = 0
       }

       if ($msms -notmatch '\d'){
           $msms = 0
       }
      
       #### 
       #Get date raw file date
       $date = Invoke-Expression -Command ".\Program.exe $des_folder$filename"
            

       $all = "$date,$filename,$protein,$peptides,$psms,$msms"
       $getrequest = $all.Replace('"',"")
       
       Write-Host $getrequest

       Add-Content "\\172.25.100.115\proteomics\Automation_scripts\HeLa_${instrument}_data.csv" $getrequest
       Add-Content "\\172.25.101.24\share\HeLa_QC\HeLa_${instrument}_data.csv" $getrequest
              
       ##
       #Copy the file to local harddrive
       copy-item  $Stat_files $des_folder$instrument"_ResultStatistics/"
       
       Start-Sleep 60
       Remove-Item $des_folder"*$filename*"
       
     }
}
Write-Host "Done!"
