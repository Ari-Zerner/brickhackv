import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { HttpClientModule } from '@angular/common/http'

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { LoginComponent } from './login/login.component';
import { CreateUserComponent } from './create-user/create-user.component';
import { DashboardComponent } from './dashboard/dashboard.component';
import { CreateDebateComponent } from './create-debate/create-debate.component';
import { DebateComponent } from './debate/debate.component';
import { TopbarComponent } from './topbar/topbar.component';
import { DebatePreviewComponent } from './debate-preview/debate-preview.component';
import { VoteDistributionComponent } from './vote-distribution/vote-distribution.component';
import { OpinionVotingComponent } from './opinion-voting/opinion-voting.component';

@NgModule({
  declarations: [
    AppComponent,
    LoginComponent,
    CreateUserComponent,
    DashboardComponent,
    CreateDebateComponent,
    DebateComponent,
    TopbarComponent,
    DebatePreviewComponent,
    VoteDistributionComponent,
    OpinionVotingComponent
  ],
  imports: [
	BrowserModule,
	HttpClientModule,
    AppRoutingModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
