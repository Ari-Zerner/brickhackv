import { Component, OnInit, Input } from '@angular/core';
import { Debate } from '../models/debate';
import { UserService } from '../services/user.service';

@Component({
  selector: 'app-vote-distribution',
  templateUrl: './vote-distribution.component.html',
  styleUrls: ['./vote-distribution.component.scss']
})
export class VoteDistributionComponent implements OnInit {

	@Input() debate:Debate;

	constructor(private userService:UserService) { }

	ngOnInit() {
		// place the dots on the popularity scale (this will mostly be done in the UI)
	}

}
