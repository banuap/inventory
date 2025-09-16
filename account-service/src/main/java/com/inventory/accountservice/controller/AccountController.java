package com.inventory.accountservice.controller;

import com.inventory.accountservice.dto.AccountDto;
import com.inventory.accountservice.entity.AccountType;
import com.inventory.accountservice.service.AccountService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.Optional;

@RestController
@RequestMapping("/api/accounts")
@CrossOrigin(origins = "*")
public class AccountController {

    @Autowired
    private AccountService accountService;

    @GetMapping
    public ResponseEntity<List<AccountDto>> getAllAccounts() {
        List<AccountDto> accounts = accountService.getAllAccounts();
        return ResponseEntity.ok(accounts);
    }

    @GetMapping("/{id}")
    public ResponseEntity<AccountDto> getAccountById(@PathVariable Long id) {
        Optional<AccountDto> account = accountService.getAccountById(id);
        return account.map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }

    @GetMapping("/email/{email}")
    public ResponseEntity<AccountDto> getAccountByEmail(@PathVariable String email) {
        Optional<AccountDto> account = accountService.getAccountByEmail(email);
        return account.map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }

    @GetMapping("/type/{accountType}")
    public ResponseEntity<List<AccountDto>> getAccountsByType(@PathVariable AccountType accountType) {
        List<AccountDto> accounts = accountService.getAccountsByType(accountType);
        return ResponseEntity.ok(accounts);
    }

    @GetMapping("/search")
    public ResponseEntity<List<AccountDto>> searchAccountsByName(@RequestParam String name) {
        List<AccountDto> accounts = accountService.searchAccountsByName(name);
        return ResponseEntity.ok(accounts);
    }

    @PostMapping
    public ResponseEntity<?> createAccount(@Valid @RequestBody AccountDto accountDto) {
        try {
            AccountDto createdAccount = accountService.createAccount(accountDto);
            return ResponseEntity.status(HttpStatus.CREATED).body(createdAccount);
        } catch (RuntimeException e) {
            return ResponseEntity.status(HttpStatus.CONFLICT)
                    .body(Map.of("error", e.getMessage()));
        }
    }

    @PutMapping("/{id}")
    public ResponseEntity<?> updateAccount(@PathVariable Long id, @Valid @RequestBody AccountDto accountDto) {
        try {
            Optional<AccountDto> updatedAccount = accountService.updateAccount(id, accountDto);
            return updatedAccount.map(ResponseEntity::ok)
                    .orElse(ResponseEntity.notFound().build());
        } catch (RuntimeException e) {
            return ResponseEntity.status(HttpStatus.CONFLICT)
                    .body(Map.of("error", e.getMessage()));
        }
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteAccount(@PathVariable Long id) {
        if (accountService.deleteAccount(id)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.notFound().build();
    }

    @GetMapping("/info")
    public ResponseEntity<Map<String, Object>> getServiceInfo() {
        return ResponseEntity.ok(Map.of(
                "service", "Account Opening Microservice",
                "version", "1.0.0",
                "description", "Spring Boot microservice for account opening",
                "endpoints", Map.of(
                        "accounts", "/api/accounts",
                        "account_by_id", "/api/accounts/{id}",
                        "account_by_email", "/api/accounts/email/{email}",
                        "accounts_by_type", "/api/accounts/type/{accountType}",
                        "search_accounts", "/api/accounts/search?name={name}"
                ),
                "supported_account_types", AccountType.values()
        ));
    }
}