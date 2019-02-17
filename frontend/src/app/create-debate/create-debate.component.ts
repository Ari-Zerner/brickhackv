import { Component, OnInit, Input } from '@angular/core';
import { Router } from '@angular/router';
import { Debate } from '../models/debate';
import { UserService } from '../services/user.service';
import { routerNgProbeToken } from '@angular/router/src/router_module';

@Component({
  selector: 'app-create-debate',
  templateUrl: './create-debate.component.html',
  styleUrls: ['./create-debate.component.scss']
})
export class CreateDebateComponent implements OnInit {

	@Input() debate = new Debate();

	constructor(private userService:UserService,private router:Router) { }

	ngOnInit() {
	}

	createDebate(){
		this.userService.createDebate(this.debate).subscribe((_)=>{
			if(_.id!=null){
				this.router.navigateByUrl("/debate/"+_.id);
			}else{
				// TODO message: Debate creation Failed
				console.log("Debate creation Failed");
			}
		});
	}

	backToDashboard(){
		this.router.navigateByUrl("/dashboard");
	}

	
}
