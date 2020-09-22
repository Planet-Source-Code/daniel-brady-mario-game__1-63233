VERSION 5.00
Begin VB.Form frmGame 
   Caption         =   "Mario Game"
   ClientHeight    =   4320
   ClientLeft      =   60
   ClientTop       =   3195
   ClientWidth     =   7275
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   288
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   485
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer tmrEnemyDie 
      Enabled         =   0   'False
      Interval        =   200
      Left            =   2520
      Top             =   3840
   End
   Begin VB.Timer tmrEnemy 
      Interval        =   200
      Left            =   2040
      Top             =   3840
   End
   Begin VB.Timer tmrSliding 
      Interval        =   100
      Left            =   600
      Top             =   3840
   End
   Begin VB.Timer tmrBackground 
      Enabled         =   0   'False
      Interval        =   100
      Left            =   120
      Top             =   3840
   End
   Begin VB.Timer tmrJumping 
      Enabled         =   0   'False
      Interval        =   20
      Left            =   1560
      Top             =   3840
   End
   Begin VB.Timer tmrWalking 
      Enabled         =   0   'False
      Interval        =   100
      Left            =   1080
      Top             =   3840
   End
   Begin VB.Label lblScore 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Score: 0"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   255
      Left            =   2640
      TabIndex        =   1
      Top             =   180
      Width           =   1575
   End
   Begin VB.Label lblLives 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "x 3"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   255
      TabIndex        =   0
      Top             =   390
      Width           =   735
   End
   Begin VB.Image Image1 
      Height          =   255
      Left            =   240
      Picture         =   "frmGame.frx":0000
      Top             =   120
      Width           =   765
   End
   Begin VB.Image imgEnemy 
      Height          =   240
      Left            =   7290
      Picture         =   "frmGame.frx":0146
      Top             =   3000
      Width           =   240
   End
   Begin VB.Image imgMario 
      Height          =   480
      Left            =   2880
      Picture         =   "frmGame.frx":01DD
      Top             =   2760
      Width           =   240
   End
   Begin VB.Image imgGround 
      Height          =   2385
      Left            =   -240
      Picture         =   "frmGame.frx":02E6
      Top             =   3240
      Width           =   14460
   End
   Begin VB.Image imgBackground 
      Height          =   6360
      Left            =   -240
      Picture         =   "frmGame.frx":259FE
      Top             =   -3120
      Width           =   16605
   End
End
Attribute VB_Name = "frmGame"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim direction As Integer
Dim step As Integer
Dim down As Boolean
Dim up As Boolean
Dim speed As Integer
Dim leftdown As Boolean
Dim rightdown As Boolean
Dim jumpdirection As Integer
Dim verticaldirection As String
Dim keysdisabled As Boolean
Dim backgroundposition As Integer
Dim enemy As Integer
Dim enemystep As Integer
Dim lives As Integer
Dim score As Single
Dim bouncenum As Integer
Dim bounceheight As Integer
Dim cloudstep As Integer
Dim cloud As Boolean

Private Sub Form_Load()
Randomize
speed = 5
direction = 2
backgroundposition = 1
enemy = Int(Rnd * 5) + 1
bounceheight = 124
tmrBackground.Enabled = True
enemystep = 1
cloudstep = 1
lives = 3
score = 0
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
Select Case KeyCode
Case Is = vbKeyLeft
    If down = False And up = False And keysdisabled = False Then
        direction = "1"
        jumpdirection = "1"
        tmrWalking.Enabled = True
    End If
Case Is = vbKeyRight
    If down = False And up = False And keysdisabled = False Then
        direction = "2"
        jumpdirection = "2"
        tmrWalking.Enabled = True
    End If
Case Is = vbKeyDown
    If up = False And keysdisabled = False Then
        down = True
        tmrWalking.Enabled = False
        Select Case direction
        Case Is = 1
            imgMario.Picture = LoadPicture(App.Path & "\images\" & "CrouchingLeft.gif")
        Case Is = 2
            imgMario.Picture = LoadPicture(App.Path & "\images\" & "CrouchingRight.gif")
        End Select
    End If
Case Is = vbKeyUp
    If down = False And keysdisabled = False Then
        up = True
        verticaldirection = "up"
        tmrJumping.Enabled = True
        bouncenum = 0
    End If
Case Is = 17    'Ctrl (speed)
    speed = 10
End Select
End Sub

Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)
Select Case KeyCode
Case Is = vbKeyLeft
    If down = False Then
        imgMario.Picture = LoadPicture(App.Path & "\images\" & "StandingLeft.gif")
        tmrWalking.Enabled = False
        jumpdirection = "0"
    End If
Case Is = vbKeyRight
    If down = False Then
        imgMario.Picture = LoadPicture(App.Path & "\images\" & "StandingRight.gif")
        tmrWalking.Enabled = False
        jumpdirection = "0"
    End If
Case Is = vbKeyDown
    down = False
    Select Case direction
    Case Is = 1
        imgMario.Picture = LoadPicture(App.Path & "\images\" & "StandingLeft.gif")
    Case Is = 2
        imgMario.Picture = LoadPicture(App.Path & "\images\" & "StandingRight.gif")
    End Select
Case Is = 17    'Ctrl (speed)
    speed = 5
End Select
End Sub

Private Sub tmrWalking_Timer()
Select Case direction
Case Is = 1
    imgMario.Move imgMario.Left - (speed + 2)
    Select Case step
    Case Is = 1
        imgMario.Picture = LoadPicture(App.Path & "\images\" & "WalkingLeft1.gif")
    Case Is = 2
        imgMario.Picture = LoadPicture(App.Path & "\images\" & "WalkingLeft2.gif")
    Case Is = 3
        imgMario.Picture = LoadPicture(App.Path & "\images\" & "WalkingLeft1.gif")
    Case Is = 4
        imgMario.Picture = LoadPicture(App.Path & "\images\" & "StandingLeft.gif")
    End Select
Case Is = 2
    imgMario.Move imgMario.Left + speed
    Select Case step
    Case Is = 1
        imgMario.Picture = LoadPicture(App.Path & "\images\" & "WalkingRight1.gif")
    Case Is = 2
        imgMario.Picture = LoadPicture(App.Path & "\images\" & "WalkingRight2.gif")
    Case Is = 3
        imgMario.Picture = LoadPicture(App.Path & "\images\" & "WalkingRight1.gif")
    Case Is = 4
        imgMario.Picture = LoadPicture(App.Path & "\images\" & "StandingRight.gif")
    End Select
End Select
step = step + 1
If step > 4 Then
    step = 1
End If
End Sub

Private Sub tmrJumping_Timer()
keysdisabled = True
Select Case direction
Case Is = 1
    imgMario.Picture = LoadPicture(App.Path & "\images\" & "JumpingLeftUp.gif")
Case Is = 2
    imgMario.Picture = LoadPicture(App.Path & "\images\" & "JumpingRightUp.gif")
End Select
Select Case jumpdirection
Case Is = 0
    If verticaldirection = "up" Then
        If imgMario.Top >= bounceheight Then
            imgMario.Move imgMario.Left, imgMario.Top - 5
        Else
            verticaldirection = "down"
            bounceheight = 124
        End If
    Else
        imgMario.Move imgMario.Left, imgMario.Top + 5
        If imgMario.Left >= imgEnemy.Left And imgMario.Left <= (imgEnemy.Left + imgEnemy.Width) Or (imgMario.Left + imgMario.Width) >= imgEnemy.Left And imgMario.Left <= imgEnemy.Left Then
            If (imgMario.Top + imgMario.Height) >= imgEnemy.Top And imgMario.Top <= (imgEnemy.Top + imgEnemy.Height) Then
                If cloud = False Then
                    score = score + 100
                    bounceheight = bounceheight - imgEnemy.Height
                End If
                lblScore.Caption = "Score: " & score
                tmrEnemy.Enabled = False
                tmrEnemyDie.Enabled = True
                If bouncenum = 0 Then
                    verticaldirection = "up"
                    bouncenum = 1
                End If
            End If
        End If
    End If
Case Is = 1
    If verticaldirection = "up" Then
        If imgMario.Top >= bounceheight Then
            imgMario.Move imgMario.Left - 1, imgMario.Top - 5
        Else
            verticaldirection = "down"
            bounceheight = 124
        End If
    Else
        imgMario.Move imgMario.Left - 1, imgMario.Top + 5
        If imgMario.Left >= imgEnemy.Left And imgMario.Left <= (imgEnemy.Left + imgEnemy.Width) Or (imgMario.Left + imgMario.Width) >= imgEnemy.Left And imgMario.Left <= imgEnemy.Left Then
            If (imgMario.Top + imgMario.Height) >= imgEnemy.Top And imgMario.Top <= (imgEnemy.Top + imgEnemy.Height) Then
                If cloud = False Then
                    score = score + 100
                    bounceheight = bounceheight - imgEnemy.Height
                End If
                lblScore.Caption = "Score: " & score
                tmrEnemy.Enabled = False
                tmrEnemyDie.Enabled = True
                If bouncenum = 0 Then
                    verticaldirection = "up"
                    bouncenum = 1
                End If
            End If
        End If
    End If
Case Is = 2
    If verticaldirection = "up" Then
        If imgMario.Top >= bounceheight Then
            imgMario.Move imgMario.Left + 1, imgMario.Top - 5
        Else
            verticaldirection = "down"
            bounceheight = 124
        End If
    Else
        imgMario.Move imgMario.Left + 1, imgMario.Top + 5
        If imgMario.Left >= imgEnemy.Left And imgMario.Left <= (imgEnemy.Left + imgEnemy.Width) Or (imgMario.Left + imgMario.Width) >= imgEnemy.Left And imgMario.Left <= imgEnemy.Left Then
            If (imgMario.Top + imgMario.Height) >= imgEnemy.Top And imgMario.Top <= (imgEnemy.Top + imgEnemy.Height) Then
                If cloud = False Then
                    score = score + 100
                    bounceheight = bounceheight - imgEnemy.Height
                End If
                lblScore.Caption = "Score: " & score
                tmrEnemy.Enabled = False
                tmrEnemyDie.Enabled = True
                If bouncenum = 0 Then
                    verticaldirection = "up"
                    bouncenum = 1
                End If
            End If
        End If
    End If
End Select
If imgMario.Top >= 184 Then
    up = False
    tmrJumping.Enabled = False
    imgMario.Top = 184
    Select Case direction
    Case Is = 1
        imgMario.Picture = LoadPicture(App.Path & "\images\" & "StandingLeft.gif")
    Case Is = 2
        imgMario.Picture = LoadPicture(App.Path & "\images\" & "StandingRight.gif")
    End Select
    keysdisabled = False
End If
End Sub

Private Sub tmrBackground_Timer()
Select Case backgroundposition
Case Is = 1
    imgGround.Left = -16
Case Is = 2
    imgGround.Left = -18
Case Is = 3
    imgGround.Left = -20
Case Is = 4
    imgGround.Left = -22
Case Is = 5
    imgGround.Left = -24
Case Is = 6
    imgGround.Left = -26
Case Is = 7
    imgGround.Left = -28
Case Is = 8
    imgGround.Left = -30
Case Is = 9
    imgGround.Left = -32
End Select
backgroundposition = backgroundposition + 1
If backgroundposition > 9 Then
    backgroundposition = 1
End If
End Sub

Private Sub tmrSliding_Timer()
If tmrWalking.Enabled = False And tmrJumping.Enabled = False Then
    imgMario.Move imgMario.Left - 2
End If
If tmrJumping.Enabled = False And cloud = False Then
    If imgMario.Left >= imgEnemy.Left And imgMario.Left <= (imgEnemy.Left + imgEnemy.Width) Or (imgMario.Left + imgMario.Width) >= imgEnemy.Left And imgMario.Left <= imgEnemy.Left Then
        lives = lives - 1
        lblLives = lives
        If lives > 0 Then
            MsgBox "You died, try again."
        End If
        imgMario.Left = 192
        imgEnemy.Left = 486
        tmrWalking.Enabled = False
    End If
End If
If lives <= 0 Then
    Unload Me
    frmGameOver.Visible = True
End If
End Sub

Private Sub tmrEnemy_Timer()
'enemy = 1 'Soon to be int(rnd*5)+1
imgEnemy.Move imgEnemy.Left - 6
Select Case enemy
Case Is = 1
    imgEnemy.Top = 200
    Select Case enemystep
    Case Is = 1
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Goomba1.gif")
    Case Is = 2
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Goomba2.gif")
    End Select
    enemystep = enemystep + 1
    If enemystep > 2 Then
        enemystep = 1
    End If
Case Is = 2
    imgEnemy.Top = 200
    Select Case enemystep
    Case Is = 1
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Worm1.gif")
    Case Is = 2
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Worm2.gif")
    End Select
    enemystep = enemystep + 1
    If enemystep > 2 Then
        enemystep = 1
    End If
Case Is = 3
    imgEnemy.Top = 184
    Select Case enemystep
    Case Is = 1
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Dragon1.gif")
    Case Is = 2
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Dragon2.gif")
    End Select
    enemystep = enemystep + 1
    If enemystep > 2 Then
        enemystep = 1
    End If
Case Is = 4
    imgEnemy.Top = 189
    Select Case enemystep
    Case Is = 1
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Turtle1.gif")
    Case Is = 2
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Turtle2.gif")
    End Select
    enemystep = enemystep + 1
    If enemystep > 2 Then
        enemystep = 1
    End If
Case Is = 5
    imgEnemy.Top = 198
    imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "BulletBill.gif")
End Select
End Sub

Private Sub tmrEnemyDie_Timer()
Select Case enemy
Case Is = 1
    cloud = True
    imgEnemy.Top = 200
    Select Case cloudstep
    Case Is = 1
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Smoke1.gif")
    Case Is = 2
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Smoke2.gif")
    Case Is = 3
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Smoke3.gif")
    End Select
    cloudstep = cloudstep + 1
    If cloudstep > 3 Then
        enemy = Int(Rnd * 5) + 1
        tmrEnemyDie.Enabled = False
        tmrEnemy.Enabled = True
        imgEnemy.Left = 486
        cloudstep = 1
        cloud = False
    End If
Case Is = 2
    cloud = True
    imgEnemy.Top = 200
    Select Case cloudstep
    Case Is = 1
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Smoke1.gif")
    Case Is = 2
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Smoke2.gif")
    Case Is = 3
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Smoke3.gif")
    End Select
    cloudstep = cloudstep + 1
    If cloudstep > 3 Then
        enemy = Int(Rnd * 5) + 1
        tmrEnemyDie.Enabled = False
        tmrEnemy.Enabled = True
        imgEnemy.Left = 486
        cloudstep = 1
        cloud = False
    End If
Case Is = 3
    cloud = True
    imgEnemy.Top = 195
    Select Case cloudstep
    Case Is = 1
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Smoke1.gif")
    Case Is = 2
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Smoke2.gif")
    Case Is = 3
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Smoke3.gif")
    End Select
    cloudstep = cloudstep + 1
    If cloudstep > 3 Then
        enemy = Int(Rnd * 5) + 1
        tmrEnemyDie.Enabled = False
        tmrEnemy.Enabled = True
        imgEnemy.Left = 486
        cloudstep = 1
        cloud = False
    End If
Case Is = 4
    cloud = True
    imgEnemy.Top = 198
    Select Case cloudstep
    Case Is = 1
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Smoke1.gif")
    Case Is = 2
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Smoke2.gif")
    Case Is = 3
        imgEnemy.Picture = LoadPicture(App.Path & "\images\" & "Smoke3.gif")
    End Select
    cloudstep = cloudstep + 1
    If cloudstep > 3 Then
        enemy = Int(Rnd * 5) + 1
        tmrEnemyDie.Enabled = False
        tmrEnemy.Enabled = True
        imgEnemy.Left = 486
        cloudstep = 1
        cloud = False
    End If
Case Is = 5
    cloud = True
    imgEnemy.Move imgEnemy.Left, imgEnemy.Top + 8
    If imgEnemy.Top >= 300 Then
        enemy = Int(Rnd * 5) + 1
        imgEnemy.Top = 198
        tmrEnemyDie.Enabled = False
        tmrEnemy.Enabled = True
        imgEnemy.Left = 486
        cloud = False
    End If
End Select
End Sub
