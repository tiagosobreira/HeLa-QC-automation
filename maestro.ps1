param ($instrument)
##################################################################################
#       Copy raw files from MS computer to proteomics file server                #
#                                                                                #
#                        How to run the script                                   #
#                                                                                #
# powershell.exe -ExecutionPolicy Bypass .\maestro.ps1 -instrument Eclipse_1     #
##################################################################################

$current_date = Get-Date


Add-Content -Path .\log_maestro.txt -Value "$current_date MS $instrument" -encoding UTF8

#####
#Only copy files newer than 7 days
$start_time=(Get-Date) - (new-timespan -day 7)
$end_time= Get-Date
    
###
#Mass spectrometry instrument HeLa raw files folder
$folder = "D:\HeLa_QC\*.raw"

###
#proteomics files server HeLa raw files folder
$des_folder = "\\172.25.100.115\proteomics\Automation_scripts\Hela_"+$instrument+"\"

###
#loop through all files a copy any new one
Get-ChildItem $folder -Recurse | foreach {
    if($_.lastwritetime -ge $start_time -and $_.lastwritetime -le $end_time) { 
        $filename = (Get-Item $_).Basename

        ##
        #next if the files is already on the server
        if (Test-Path $des_folder$filename".raw"){
            return
        }

        ##
        #if files is smaller than 100,000 is probably still collecting the data
        #if it is larger than verify if the file change after 400 seconds
        $size1 = ((Get-Item $_).length/1KB)
        if ($size1 -lt 100000){
            return
        }

        Start-Sleep 400

        ##
        #if the file size changed it is still collecting the data
        $size2 = ((Get-Item $_).length/1KB)
        if ($size2 -gt $size1){
            Write-Host "MS run is NOT complete!"
            return
        }
        
        ##
        #copy the new raw file to the server
        copy-item  $_.fullname $des_folder
    }
}