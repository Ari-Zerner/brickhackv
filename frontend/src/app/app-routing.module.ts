import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

import { DashboardComponent } from './dashboard/dashboard.component';
import { LoginComponent } from './login/login.component';
import { CreateDebateComponent } from './create-debate/create-debate.component';
import { DebateComponent } from './debate/debate.component';
import { CreateUserComponent } from './create-user/create-user.component';

const routes: Routes = [
	{ path: '', component: LoginComponent},
	{ path: 'dashboard', component: DashboardComponent },
	{ path: 'debate/:id', component: DebateComponent },
	{ path: 'create-user', component: CreateUserComponent },
	{ path: 'create-debate', component: CreateDebateComponent }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
